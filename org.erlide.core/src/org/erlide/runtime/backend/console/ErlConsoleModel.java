/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend.console;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.events.EventHandler;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlConsoleModel implements IDisposable {

	private static final int MAX_REQUESTS = 5000;
	private static final int DELTA_REQUESTS = 500;

	public class ConsoleEventHandler extends EventHandler {

		@Override
		protected void doHandleMsg(final OtpErlangObject msg) throws Exception {
			final OtpErlangObject event = getStandardEvent(msg, "io_server");
			if (event == null) {
				return;
			}
			// ErlLogger.debug(">>> " + event);
			add(event);
		}
	}

	private final List<IoRequest> requests;
	private final List<ErlConsoleModelListener> listeners;
	private final ConsoleEventHandler handler;
	private int pos = 0;

	public ErlConsoleModel() {
		super();
		requests = new ArrayList<IoRequest>(1000);
		listeners = new ArrayList<ErlConsoleModelListener>();
		handler = new ConsoleEventHandler();
	}

	public List<IoRequest> getContentList() {
		synchronized (requests) {
			return new ArrayList<IoRequest>(requests);
		}
	}

	public void input(String s) {
		if (!s.endsWith("\n")) {
			s += "\n";
		}
		final IoRequest req = new IoRequest(s);
		req.setStart(pos);
		pos += req.getLength();
		requests.add(req);
		notifyListeners();
	}

	public void add(final OtpErlangObject msg) {
		synchronized (requests) {
			deleteOldItems();
			final IoRequest req = doAdd(msg);
			if (req == null) {
				return;
			}
		}
		notifyListeners();
	}

	private IoRequest doAdd(final OtpErlangObject msg) {
		if (!(msg instanceof OtpErlangTuple)) {
			return null;
		}
		final IoRequest req = new IoRequest((OtpErlangTuple) msg);
		req.setStart(pos);
		pos += req.getLength();

		requests.add(req);

		return req;
	}

	private void deleteOldItems() {
		// TODO use a configuration for this
		// TODO maybe we should count text lines?
		if (requests.size() > MAX_REQUESTS) {
			requests.subList(0, DELTA_REQUESTS).clear();
			IoRequest first = requests.get(0);
			int start = first.getStart();
			for (IoRequest areq : requests) {
				areq.setStart(areq.getStart() - start);
			}
		}
	}

	public IoRequest findAtPos(final int pos) {
		synchronized (requests) {
			for (final IoRequest req : requests) {
				if (req.getStart() <= pos
						&& req.getStart() + req.getLength() > pos) {
					return req;
				}
			}
			return null;
		}
	}

	public List<IoRequest> getAllFrom(final OtpErlangPid sender) {
		final List<IoRequest> result = new ArrayList<IoRequest>();
		for (final IoRequest element : requests) {
			if (element.getSender().equals(sender)) {
				result.add(element);
			}
		}
		return result;
	}

	public void add(final List<OtpErlangObject> msgs) {
		synchronized (requests) {
			deleteOldItems();
			for (final OtpErlangObject element : msgs) {
				doAdd(element);
			}
		}
		notifyListeners();
	}

	public void dispose() {
		listeners.clear();
	}

	public synchronized void addListener(final ErlConsoleModelListener listener) {
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}

	public synchronized void removeListener(
			final ErlConsoleModelListener listener) {
		listeners.remove(listener);
	}

	private void notifyListeners() {
		for (final ErlConsoleModelListener listener : listeners) {
			listener.changed(this);
		}
	}

	public ConsoleEventHandler getHandler() {
		return handler;
	}

	public int getTextLength() {
		synchronized (requests) {
			int res = 0;
			for (IoRequest req : getContentList()) {
				res += req.getLength();
			}
			return res;
		}
	}

	public String getText() {
		synchronized (requests) {
			String res = "";
			for (IoRequest req : getContentList()) {
				res += req.getMessage();
			}
			return res;
		}
	}

}
