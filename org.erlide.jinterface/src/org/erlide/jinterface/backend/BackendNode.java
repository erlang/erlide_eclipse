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
package org.erlide.jinterface.backend;

import java.io.IOException;
import java.net.Socket;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.ericsson.otp.erlang.SignatureException;

public class BackendNode {

	private static final OtpErlangAtom GLEADER = new OtpErlangAtom("user");
	private static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
			"erlide.connect.delay", "300"));
	private static final int EPMD_PORT = 4369;

	protected boolean available = false;
	protected OtpMbox eventBox;
	protected OtpNode fNode;
	protected String fPeer;
	private String currentVersion;
	protected final RuntimeInfo fInfo;
	private boolean fDebug;
	protected int exitStatus = -1;
	private boolean stopped = false;
	protected int restarted = 0;

	public void connect() {
		doConnect(getName());
	}

	public boolean ping() {
		return fNode.ping(fPeer, 500);
	}

	@SuppressWarnings("boxing")
	public void doConnect(final String label) {
		ErlLogger.debug("connect to:: '" + label + "' "
				+ Thread.currentThread());
		// Thread.dumpStack();
		try {
			wait_for_epmd();

			final String cookie = getInfo().getCookie();
			if (cookie == null) {
				fNode = new OtpNode(BackendUtil.createJavaNodeName());
			} else {
				fNode = new OtpNode(BackendUtil.createJavaNodeName(), cookie);
			}
			final String nodeCookie = fNode.cookie();
			final int len = nodeCookie.length();
			final String trimmed = len > 7 ? nodeCookie.substring(0, 7)
					: nodeCookie;
			ErlLogger.debug("using cookie '%s...'%d (info: '%s')", trimmed,
					len, cookie);
			fPeer = BackendUtil.buildNodeName(label, true);

			eventBox = fNode.createMbox("rex");
			int tries = 20;
			while (!available && tries > 0) {
				available = fNode.ping(fPeer, RETRY_DELAY + (20 - tries)
						* RETRY_DELAY / 5);
				tries--;
			}
			available &= waitForCodeServer();

			if (available) {
				ErlLogger.debug("connected!");
			} else {
				ErlLogger
						.error("could not connect to backend! Please check runtime settings.");
			}

		} catch (final Exception e) {
			ErlLogger.error(e);
			available = false;
			ErlLogger
					.error("could not connect to backend! Please check runtime settings.");
		}
	}

	public void dispose() {
		dispose(false);
	}

	public void dispose(final boolean restart) {
		ErlLogger.debug("disposing backend " + getName());

		if (fNode != null) {
			fNode.close();
		}

		if (restart) {
			return;
		}
	}

	public synchronized void restart() {
		exitStatus = -1;
		if (available) {
			return;
		}
		restarted++;
		ErlLogger.info("restarting runtime for %s", toString());
		if (fNode != null) {
			fNode.close();
			fNode = null;
		}
		initializeRuntime();
		connect();
		initErlang();
	}

	private OtpMbox getEventBox() {
		return eventBox;
	}

	public OtpErlangPid getEventPid() {
		final OtpMbox eventBox = getEventBox();
		if (eventBox == null) {
			return null;
		}
		return eventBox.self();
	}

	public String getCurrentVersion() {
		if (currentVersion == null) {
			try {
				currentVersion = getScriptId();
			} catch (final Exception e) {
			}
		}
		return currentVersion;
	}

	public BackendNode(RuntimeInfo info) throws BackendException {
		if (info == null) {
			throw new BackendException(
					"Can't create backend without runtime information");
		}
		fInfo = info;
	}

	protected void wait_for_epmd() throws BackendException {
		// If anyone has a better solution for waiting for epmd to be up, please
		// let me know
		int tries = 50;
		boolean ok = false;
		do {
			Socket s;
			try {
				s = new Socket("localhost", EPMD_PORT);
				s.close();
				ok = true;
			} catch (final IOException e) {
			}
			try {
				Thread.sleep(100);
				// ErlLogger.debug("sleep............");
			} catch (final InterruptedException e1) {
			}
			tries--;
		} while (!ok && tries > 0);
		if (!ok) {
			final String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
					+ "  Possibly your host's entry in /etc/hosts is wrong.";
			ErlLogger.error(msg);
			throw new BackendException(msg);
		}
	}

	public void initErlang() {
		final boolean inited = init(getEventPid());
		if (!inited) {
			setAvailable(false);
		}
	}

	public RuntimeInfo getInfo() {
		return fInfo;
	}

	public String getName() {
		if (fInfo == null) {
			return "<not_connected>";
		}
		return fInfo.getNodeName();
	}

	public String getPeer() {
		return fPeer;
	}

	public void initializeRuntime() {
		dispose(true);
	}

	protected void setRemoteRex(final OtpErlangPid watchdog) {
		try {
			getEventBox().link(watchdog);
		} catch (final OtpErlangExit e) {
		}
	}

	public void setAvailable(final boolean up) {
		available = up;
	}

	public void setDebug(final boolean b) {
		fDebug = b;
	}

	public boolean isDebug() {
		return fDebug;
	}

	public String getJavaNodeName() {
		return fNode.node();
	}

	public void stop() {
		stopped = true;
	}

	public void setExitStatus(final int v) {
		exitStatus = v;
	}

	public boolean isStopped() {
		return stopped;
	}

	public void registerStatusHandler(OtpNodeStatus handler) {
		fNode.registerStatusHandler(handler);
	}

	private boolean init(final OtpErlangPid jRex) {
		try {
			// reload(backend);
			RpcUtil.rpcCall(fNode, fPeer, GLEADER, "erlide_backend", "init",
					9000, "p", jRex);
			return true;
		} catch (final Exception e) {
			ErlLogger.error(e);
			return false;
		}
	}

	private String getScriptId() throws BackendException {
		OtpErlangObject r;
		try {
			r = RpcUtil.rpcCall(fNode, fPeer, GLEADER, "init", "script_id",
					9000, "");
			if (r instanceof OtpErlangTuple) {
				final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
				if (rr instanceof OtpErlangString) {
					return ((OtpErlangString) rr).stringValue();
				}
			}
			return "";
		} catch (RpcException e) {
			throw new BackendException(e);
		} catch (SignatureException e) {
			throw new BackendException(e);
		}
	}

	private boolean waitForCodeServer() {
		try {
			OtpErlangObject r;
			int i = 10;
			do {
				r = RpcUtil.rpcCall(fNode, fPeer, GLEADER, "erlang", "whereis",
						9000, "a", "code_server");
				Thread.sleep(200);
				i--;
			} while (!(r instanceof OtpErlangPid) && i > 0);
			if (!(r instanceof OtpErlangPid)) {
				ErlLogger.error("code server did not start in time for %s",
						getInfo().getName());
				return false;
			}
			ErlLogger.debug("code server started");
			return true;
		} catch (final Exception e) {
			ErlLogger.error("error starting code server for %s: %s", getInfo()
					.getName(), e.getMessage());
			return false;
		}
	}

}