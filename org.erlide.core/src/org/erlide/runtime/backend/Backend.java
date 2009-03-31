/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.erlide.core.ErlangProjectProperties;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.IDisposable;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.console.IShellManager;
import org.erlide.runtime.backend.events.EventDaemon;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.internal.CodeManager;
import org.erlide.runtime.backend.internal.LogEventHandler;
import org.erlide.runtime.backend.internal.RuntimeLauncher;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;

import erlang.ErlangCode;
import erlang.ErlideBackend;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class Backend extends OtpNodeStatus implements IDisposable {

	private static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
			"erlide.connect.delay", "250"));

	private static int DEFAULT_TIMEOUT;
	{
		String t = System.getProperty("erlide.rpc.timeout", "9000");
		if ("infinity".equals(t)) {
			DEFAULT_TIMEOUT = RpcUtil.INFINITY;
		} else {
			DEFAULT_TIMEOUT = Integer.parseInt(t);
		}
	}

	private final CodeManager fCodeManager;
	boolean fAvailable = false;

	private OtpMbox ftRpcBox; // incoming rpc and events
	private OtpNode fNode;
	private String fPeer;
	private IShellManager fShellManager;
	private String fCurrentVersion;
	private final RuntimeInfo fInfo;
	private boolean fDebug;
	private EventDaemon eventDaemon;
	private final RuntimeLauncher launcher;
	private boolean trapexit;
	private int exitStatus = -1;
	private boolean stopped = false;

	Backend(final RuntimeInfo info, final RuntimeLauncher launcher)
			throws BackendException {
		if (info == null) {
			throw new BackendException(
					"Can't create backend without runtime information");
		}
		fCodeManager = new CodeManager(this);
		fShellManager = new BackendShellManager(this);
		fInfo = info;
		this.launcher = launcher;
		this.launcher.setBackend(this);
	}

	public void connect() {
		launcher.connect();
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
				fNode = new OtpNode(BackendManager.getJavaNodeName());
			} else {
				fNode = new OtpNode(BackendManager.getJavaNodeName(), cookie);
			}
			final String nodeCookie = fNode.cookie();
			final int len = nodeCookie.length();
			final String trimmed = len > 7 ? nodeCookie.substring(0, 7)
					: nodeCookie;
			ErlLogger.debug("using cookie '%s...'%d (info: '%s')", trimmed,
					len, cookie);
			fNode.registerStatusHandler(this);
			fPeer = BackendManager.buildNodeName(label);

			ftRpcBox = fNode.createMbox("rex");
			int tries = 50;
			while (!fAvailable && tries > 0) {
				fAvailable = fNode.ping(fPeer, RETRY_DELAY + (50 - tries)
						* RETRY_DELAY / 20);
				tries--;
			}
			fAvailable &= ErlangCode.waitForCodeServer(this);

			if (fAvailable) {
				ErlLogger.debug("connected!");
			} else {
				ErlLogger
						.error("could not connect to backend! Please check runtime settings.");
			}

		} catch (final Exception e) {
			ErlLogger.error(e);
			fAvailable = false;
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
		if (fShellManager instanceof IDisposable) {
			((IDisposable) fShellManager).dispose();
		}
		if (eventDaemon != null) {
			eventDaemon.stop();
		}

		if (restart) {
			return;
		}
		if (launcher instanceof IDisposable) {
			((IDisposable) launcher).dispose();
		}
	}

	public EventDaemon getEventDaemon() {
		return eventDaemon;
	}

	/**
	 * @see Backend#call
	 * @deprecated
	 */
	@Deprecated
	public RpcResult rpc(final String m, final String f,
			final String signature, final Object... a) throws RpcException {
		return call_noexception(m, f, signature, a);
	}

	/**
	 * typed RPC
	 * 
	 */
	public RpcResult call_noexception(final String m, final String f,
			final String signature, final Object... a) throws RpcException {
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
			OtpErlangObject result = makeCall(timeout, m, f, signature, args);
			return new RpcResult(result);
		} catch (RpcException e) {
			return RpcResult.error(e.getMessage());
		}
	}

	public OtpMbox async_call(final String m, final String f,
			final String signature, final Object... args) throws RpcException {
		return makeAsyncCall(m, f, signature, args);
	}

	public OtpErlangObject async_receive(OtpMbox mbox, int timeout)
			throws RpcException {
		return RpcUtil.getRpcResult(mbox, timeout);
	}

	public void cast(final String m, final String f, final String signature,
			final Object... args) throws RpcException {
		makeCast(m, f, signature, args);
	}

	/**
	 * typed RPC , throws Exception
	 * 
	 */
	public OtpErlangObject call(final String m, final String f,
			final String signature, final Object... a) throws RpcException {
		return call(DEFAULT_TIMEOUT, m, f, signature, a);
	}

	/**
	 * @see Backend#callx
	 * @deprecated
	 */
	@Deprecated
	public OtpErlangObject rpcx(final String m, final String f,
			final String signature, final Object... a) throws RpcException {
		return call(m, f, signature, a);
	}

	/**
	 * typed RPC with timeout, throws Exception
	 * 
	 * @throws ConversionException
	 */
	public OtpErlangObject call(final int timeout, final String m,
			final String f, final String signature, final Object... a)
			throws RpcException {
		return makeCall(timeout, m, f, signature, a);
	}

	/**
	 * @see Backend#callx
	 * @deprecated
	 */
	@Deprecated
	public OtpErlangObject rpcx(final String m, final String f,
			final int timeout, final String signature, final Object... a)
			throws RpcException {
		return call(timeout, m, f, signature, a);
	}

	/**
	 * 
	 * @param msg
	 * @param dbgPid
	 * @throws ConversionException
	 */
	public void send(final OtpErlangPid pid, final Object msg) {
		if (!fAvailable) {
			return;
		}
		try {
			RpcUtil.send(fNode, pid, msg);
		} catch (final RpcException e) {
			// shouldn't happen
			ErlLogger.warn(e);
		}
	}

	public void send(final String name, final Object msg) {
		if (!fAvailable) {
			return;
		}
		try {
			RpcUtil.send(fNode, fPeer, name, msg);
		} catch (final RpcException e) {
			// shouldn't happen
			ErlLogger.warn(e);
		}
	}

	CodeManager getCodeManager() {
		return fCodeManager;
	}

	private OtpErlangObject makeCall(final int timeout, final String module,
			final String fun, final String signature, final Object... args0)
			throws RpcException {
		checkAvailability();
		OtpErlangObject result = RpcUtil.rpcCall(fNode, fPeer, module, fun,
				timeout, signature, args0);
		return result;
	}

	private OtpMbox makeAsyncCall(final String module, final String fun,
			final String signature, final Object... args0) throws RpcException {
		checkAvailability();
		return RpcUtil.sendRpcCall(fNode, fPeer, module, fun, signature, args0);
	}

	private void checkAvailability() throws RpcException {
		if (!fAvailable) {
			if (exitStatus >= 0) {
				restart();
			} else {
				throw new RpcException("could not restart backend");
			}
		}
	}

	private void makeCast(final String module, final String fun,
			final String signature, final Object... args0) throws RpcException {
		checkAvailability();
		RpcUtil.rpcCast(fNode, fPeer, module, fun, signature, args0);
	}

	private synchronized void restart() {
		exitStatus = -1;
		if (fAvailable) {
			return;
		}
		ErlLogger.info("restarting runtime for %s", toString());
		if (fNode != null) {
			fNode.close();
			fNode = null;
		}
		initializeRuntime(null);
		final Collection<ICodeBundle> plugins = ErlangCore.getBackendManager()
				.getPlugins();
		for (final ICodeBundle bundle : plugins) {
			getCodeManager().unregister(bundle);
		}
		connectAndRegister(plugins);
		initErlang();
	}

	private OtpMbox getEventBox() {
		return ftRpcBox;
	}

	public OtpErlangPid getEventPid() {
		final OtpMbox eventBox = getEventBox();
		if (eventBox == null) {
			return null;
		}
		return eventBox.self();
	}

	public String getCurrentVersion() {
		if (fCurrentVersion == null) {
			try {
				fCurrentVersion = ErlideBackend.getScriptId(this);
			} catch (final Exception e) {
			}
		}
		return fCurrentVersion;
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
		final OtpMbox eventBox = getEventBox();
		if (eventBox == null) {
			return null;
		}
		return eventBox.receive(timeout);
	}

	public IShellManager getShellManager() {
		return fShellManager;
	}

	public void initErlang() {
		final boolean inited = ErlideBackend.init(this, getEventPid());
		if (!inited) {
			setAvailable(false);
		}
		eventDaemon = new EventDaemon(this);
		eventDaemon.start();

		eventDaemon.addListener(new LogEventHandler());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.runtime.backend.Backend#getInfo()
	 */
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

	public void initializeRuntime(final ILaunch launch) {
		dispose(true);
		launcher.initializeRuntime(launch);
		fShellManager = new BackendShellManager(this);
	}

	public void setRemoteRex(final OtpErlangPid watchdog) {
		try {
			getEventBox().link(watchdog);
		} catch (final OtpErlangExit e) {
		}
	}

	public void connectAndRegister(final Collection<ICodeBundle> plugins) {
		connect();
		if (plugins != null) {
			for (final ICodeBundle element : plugins) {
				getCodeManager().register(element);
			}
		}
		checkCodePath();
	}

	public void checkCodePath() {
		try {
			call("code", "get_path", "");
		} catch (final Throwable e) {
			ErlLogger.warn("error getting path for %s: %s", getName(), e
					.getMessage());
		}
	}

	@Override
	public void remoteStatus(final String node, final boolean up,
			final Object info) {
		// final String dir = up ? "up" : "down";
		// ErlLogger.debug(String.format("@@: %s %s %s", node, dir, info));
		if (node.equals(fPeer)) {
			setAvailable(up);
		}
		ErlangCore.getBackendManager().remoteNodeStatus(node, up, info);
	}

	private void setAvailable(final boolean up) {
		// TODO notify others? BackendManager?
		fAvailable = up;
	}

	@Override
	public void connAttempt(final String node, final boolean incoming,
			final Object info) {
		final String direction = incoming ? "in" : "out";
		ErlLogger.info(String.format("Connection attempt: %s %s: %s", node,
				direction, info));
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

	public void removePath(final boolean usePathZ, final String path) {
		fCodeManager.removePath(usePathZ, path);
	}

	public void addPath(final boolean usePathZ, final String path) {
		fCodeManager.addPath(usePathZ, path);
	}

	public void registerProjects(final Collection<IProject> projects) {
		for (final IProject project : projects) {
			ErlangCore.getBackendManager().addExecution(project, this);
			final ErlangProjectProperties prefs = ErlangCore
					.getProjectProperties(project);
			final String outDir = project.getLocation().append(
					prefs.getOutputDir()).toOSString();
			if (outDir.length() > 0) {
				ErlLogger.debug("backend %s: add path %s", getName(), outDir);
				addPath(false/* prefs.getUsePathZ() */, outDir);
				File f = new File(outDir);
				for (File b : f.listFiles()) {
					ErlangCode.load(this, b.getName());
				}
			}
		}

	}

	public void setTrapExit(final boolean trapexit) {
		this.trapexit = trapexit;
	}

	public boolean getTrapExit() {
		return trapexit;
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

}
