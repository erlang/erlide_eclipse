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
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.ericsson.otp.erlang.SignatureException;

public class Backend {

	private static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
			"erlide.connect.delay", "300"));

	private static int DEFAULT_TIMEOUT;
	{
		final String t = System.getProperty("erlide.rpc.timeout", "9000");
		if ("infinity".equals(t)) {
			DEFAULT_TIMEOUT = RpcUtil.INFINITY;
		} else {
			DEFAULT_TIMEOUT = Integer.parseInt(t);
		}
	}

	boolean available = false;

	private OtpMbox eventBox; // incoming rpc and events
	private OtpNode fNode;
	private String fPeer;
	private String currentVersion;
	private final RuntimeInfo fInfo;
	private boolean fDebug;
	private final RuntimeLauncher launcher;
	private int exitStatus = -1;
	private boolean stopped = false;
	private int restarted = 0;

	protected Backend(final RuntimeInfo info, final RuntimeLauncher launcher)
	throws BackendException {
		if (info == null) {
			throw new BackendException(
			"Can't create backend without runtime information");
		}
		fInfo = info;
		this.launcher = launcher;
		this.launcher.setBackend(this);
	}

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
				fNode = new OtpNode(BackendUtil.getJavaNodeName());
			} else {
				fNode = new OtpNode(BackendUtil.getJavaNodeName(), cookie);
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
			available &= ErlangCode.waitForCodeServer(this);

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
		if (launcher instanceof IDisposable) {
			((IDisposable) launcher).dispose();
		}
	}

	/**
	 * typed RPC
	 * 
	 */
	public RpcResult call_noexception(final String m, final String f,
			final String signature, final Object... a) {
		return call_noexception(DEFAULT_TIMEOUT, m, f, signature, a);
	}

	/**
	 * typed RPC with timeout
	 * 
	 * @throws ConversionException
	 */
	public RpcResult call_noexception(final int timeout, final String m,
			final String f, final String signature, final Object... args) {
		try {
			final OtpErlangObject result = makeCall(timeout, m, f, signature,
					args);
			return new RpcResult(result);
		} catch (final RpcException e) {
			return RpcResult.error(e.getMessage());
		} catch (SignatureException e) {
			return RpcResult.error(e.getMessage());
		}
	}

	public RpcFuture async_call(final String m, final String f,
			final String signature, final Object... args)
	throws BackendException {
		try {
			return makeAsyncCall(m, f, signature, args);
		} catch (final RpcException e) {
			throw new BackendException(e);
		} catch (SignatureException e) {
			throw new BackendException(e);
		}
	}

	public void cast(final String m, final String f, final String signature,
			final Object... args) throws BackendException {
		try {
			makeCast(m, f, signature, args);
		} catch (final RpcException e) {
			throw new BackendException(e);
		} catch (SignatureException e) {
			throw new BackendException(e);
		}
	}

	/**
	 * typed RPC , throws Exception
	 * 
	 */
	public OtpErlangObject call(final String m, final String f,
			final String signature, final Object... a) throws BackendException {
		return call(DEFAULT_TIMEOUT, m, f, signature, a);
	}

	/**
	 * typed RPC with timeout, throws Exception
	 * 
	 * @throws ConversionException
	 */
	public OtpErlangObject call(final int timeout, final String m,
			final String f, final String signature, final Object... a)
	throws BackendException {
		return call(timeout, new OtpErlangAtom("user"), m, f, signature, a);
	}

	public OtpErlangObject call(final int timeout,
			final OtpErlangObject gleader, final String m, final String f,
			final String signature, final Object... a) throws BackendException {
		try {
			return makeCall(timeout, gleader, m, f, signature, a);
		} catch (final RpcException e) {
			throw new BackendException(e);
		} catch (SignatureException e) {
			throw new BackendException(e);
		}
	}

	/**
	 * 
	 * @param msg
	 * @param dbgPid
	 * @throws ConversionException
	 */
	public void send(final OtpErlangPid pid, final Object msg) {
		if (!available) {
			return;
		}
		try {
			RpcUtil.send(fNode, pid, msg);
		} catch (final SignatureException e) {
			// shouldn't happen
			ErlLogger.warn(e);
		}
	}

	public void send(final String name, final Object msg) {
		if (!available) {
			return;
		}
		try {
			RpcUtil.send(fNode, fPeer, name, msg);
		} catch (final SignatureException e) {
			// shouldn't happen
			ErlLogger.warn(e);
		}
	}

	private OtpErlangObject makeCall(final int timeout, final String module,
			final String fun, final String signature, final Object... args0)
	throws RpcException, SignatureException {
		return makeCall(timeout, new OtpErlangAtom("user"), module, fun,
				signature, args0);
	}

	private OtpErlangObject makeCall(final int timeout,
			final OtpErlangObject gleader, final String module,
			final String fun, final String signature, final Object... args0)
	throws RpcException, SignatureException {
		checkAvailability();
		final OtpErlangObject result = RpcUtil.rpcCall(fNode, fPeer, gleader,
				module, fun, timeout, signature, args0);
		return result;
	}

	private RpcFuture makeAsyncCall(final String module, final String fun,
			final String signature, final Object... args0) throws RpcException,
			SignatureException {
		return makeAsyncCall(new OtpErlangAtom("user"), module, fun, signature,
				args0);
	}

	private RpcFuture makeAsyncCall(final OtpErlangObject gleader,
			final String module, final String fun, final String signature,
			final Object... args0) throws RpcException, SignatureException {
		checkAvailability();
		return RpcUtil.sendRpcCall(fNode, fPeer, gleader, module, fun,
				signature, args0);
	}

	private synchronized void checkAvailability() throws RpcException {
		if (!available) {
			if (exitStatus >= 0 && restarted < 3) {
				restart();
			} else {
				throw new RpcException("could not restart backend");
			}
		}
	}

	private void makeCast(final String module, final String fun,
			final String signature, final Object... args0)
	throws SignatureException, RpcException {
		makeCast(new OtpErlangAtom("user"), module, fun, signature, args0);
	}

	private void makeCast(final OtpErlangObject gleader, final String module,
			final String fun, final String signature, final Object... args0)
	throws SignatureException, RpcException {
		checkAvailability();
		RpcUtil.rpcCast(fNode, fPeer, gleader, module, fun, signature, args0);
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
				currentVersion = ErlBackend.getScriptId(this);
			} catch (final Exception e) {
			}
		}
		return currentVersion;
	}

	private static final int EPMD_PORT = 4369;

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

	public OtpErlangObject receiveEvent(final long timeout)
	throws OtpErlangExit, OtpErlangDecodeException {
		if (eventBox == null) {
			return null;
		}
		return eventBox.receive(timeout);
	}

	public void initErlang() {
		final boolean inited = ErlBackend.init(this, getEventPid());
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
		launcher.initializeRuntime();
	}

	protected void setRemoteRex(final OtpErlangPid watchdog) {
		try {
			getEventBox().link(watchdog);
		} catch (final OtpErlangExit e) {
		}
	}

	// TODO should this be public?
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
		launcher.stop();
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

}
