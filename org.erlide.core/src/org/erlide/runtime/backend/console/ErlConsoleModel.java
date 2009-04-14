/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.runtime.backend.console;

import java.util.ArrayList;
import java.util.List;

import org.erlide.runtime.IDisposable;
import org.erlide.runtime.backend.events.EventHandler;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlConsoleModel implements IDisposable {

	public class ConsoleEventHandler extends EventHandler {

		@Override
		protected void doHandleMsg(final OtpErlangObject msg) throws Exception {
			final OtpErlangObject event = getStandardEvent(msg, "io_server");
			if (event == null) {
				return;
			}
			// System.out.println(">>> " + event);
			add(event);
		}
	}

	private final List<IoRequest> requests;
	private final List<ErlConsoleModelListener> listeners;
	private ConsoleEventHandler handler;
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
		final IoRequest req = new IoRequest(s);
		req.setStart(pos);
		pos += req.getLength();
		requests.add(req);
		notifyListeners();
	}

	public int add(OtpErlangObject msg) {
		if (!(msg instanceof OtpErlangTuple)) {
			return 0;
		}
		final IoRequest req = new IoRequest((OtpErlangTuple) msg);
		req.setStart(pos);
		pos += req.getLength();

		// TODO use a configuration for this
		// TODO maybe we should count text lines?
		synchronized (requests) {
			if (requests.size() > 5000) {
				for (int i = 0; i < 1000; i++) {
					requests.remove(0);
				}
			}
			requests.add(req);
		}
		notifyListeners();
		return req.getLength();
	}

	public IoRequest findAtPos(int pos) {
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

	public List<IoRequest> getAllFrom(OtpErlangPid sender) {
		final List<IoRequest> result = new ArrayList<IoRequest>(10);
		for (final IoRequest element : requests) {
			if (element.getSender().equals(sender)) {
				result.add(element);
			}
		}
		return result;
	}

	public void add(List<OtpErlangObject> msgs) {
		for (final OtpErlangObject element : msgs) {
			add(element);
		}

	}

	public void dispose() {
		listeners.clear();
	}

	public synchronized void addListener(ErlConsoleModelListener listener) {
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}

	public synchronized void removeListener(ErlConsoleModelListener listener) {
		listeners.remove(listener);
	}

	private void notifyListeners() {
		for (ErlConsoleModelListener listener : listeners) {
			listener.changed(this);
		}
	}

	public ConsoleEventHandler getHandler() {
		return handler;
	}
}
