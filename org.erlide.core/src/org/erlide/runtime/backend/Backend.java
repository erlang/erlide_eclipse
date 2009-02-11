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

import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

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
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;
import org.erlide.runtime.backend.internal.CodeManager;
import org.erlide.runtime.backend.internal.RuntimeLauncher;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
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

	final private HashMap<String, ArrayList<BackendEventListener>> fEventListeners;
	private final CodeManager fCodeManager;
	boolean fAvailable = false;

	private OtpMbox ftRpcBox; // incoming rpc and events
	protected static String fHost;
	protected OtpNode fNode;
	protected String fPeer;
	protected IShellManager fShellManager;
	private String fCurrentVersion;
	private final RuntimeInfo fInfo;
	private boolean fDebug;
	private ErlRpcDaemon rpcDaemon;
	private RuntimeLauncher launcher;

	private boolean trapexit;

	public Backend(final RuntimeInfo info, final RuntimeLauncher launcher)
			throws BackendException {
		if (info == null) {
			throw new BackendException(
					"Can't create backend without runtime information");
		}
		fEventListeners = new HashMap<String, ArrayList<BackendEventListener>>();
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
				fAvailable = fNode.ping(fPeer, RETRY_DELAY);
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

	/**
	 * Method dispose
	 */
	public void dispose() {
		ErlLogger.debug("disposing backend " + getName());

		if (fNode != null) {
			fNode.close();
		}
		if (fShellManager instanceof IDisposable) {
			((IDisposable) fShellManager).dispose();
		}
		if (launcher instanceof IDisposable) {
			((IDisposable) launcher).dispose();
		}
		getRpcDaemon().stop();
	}

	private ErlRpcDaemon getRpcDaemon() {
		if (rpcDaemon == null) {
			rpcDaemon = new ErlRpcDaemon(this);
		}
		return rpcDaemon;
	}

	public void addErlRpcMessageListener(final ErlRpcMessageListener l) {
		rpcDaemon.addErlRpcMessageListener(l);
	}

	public void removeErlRpcMessageListener(final ErlRpcMessageListener l) {
		rpcDaemon.removeErlRpcMessageListener(l);
	}

	/**
	 * typed RPC
	 * 
	 * @throws NoBackendException
	 * 
	 * @throws ConversionException
	 */
	public RpcResult rpc(final String m, final String f,
			final String signature, final Object... a) throws RpcException {
		return rpc(m, f, 5000, signature, a);
	}

	/**
	 * typed RPC with timeout
	 * 
	 * @throws ConversionException
	 */
	public RpcResult rpc(final String m, final String f, final int timeout,
			final String signature, final Object... args) throws RpcException {
		return sendRpc(m, f, timeout, signature, args);
	}

	/**
	 * typed RPC , throws Exception
	 * 
	 * @throws BackendException
	 * 
	 * @throws ConversionException
	 */
	public OtpErlangObject rpcx(final String m, final String f,
			final String signature, final Object... a) throws RpcException,
			BackendException {
		return rpcx(m, f, 5000, signature, a);
	}

	/**
	 * typed RPC with timeout, throws Exception
	 * 
	 * @throws ConversionException
	 */
	public OtpErlangObject rpcx(final String m, final String f,
			final int timeout, final String signature, final Object... a)
			throws BackendException, RpcException {
		final RpcResult r = rpc(m, f, timeout, signature, a);
		if (r != null && r.isOk()) {
			return r.getValue();
		}
		if (r == null) {
			throw new NoBackendException();
		}
		final StringBuffer sa = new StringBuffer();
		for (final Object x : a) {
			String val = x.toString();
			if (x.getClass().isArray()
					&& !x.getClass().getComponentType().isPrimitive()) {
				val = Arrays.toString((Object[]) x);
			}
			sa.append("'").append(val).append("',");
		}
		final String ss = sa.toString().replaceAll("[\\r\\n]", " ");
		final String msg = String.format("%s <- %s:%s(%s) @ %s", r.getValue(),
				m, f, ss, getInfo().toString());
		throw new BackendException(msg);
	}

	/**
	 * 
	 * @param msg
	 * @param dbgPid
	 * @throws ConversionException
	 */
	public void send(final OtpErlangPid pid, final Object msg) {
		try {
			RpcUtil.send(fNode, pid, msg);
		} catch (RpcException e) {
			// shouldn't happen
			ErlLogger.warn(e);
		}
	}

	public void send(final String name, final Object msg) {
		try {
			RpcUtil.send(fNode, fPeer, name, msg);
		} catch (final RpcException e) {
			// shouldn't happen
			ErlLogger.warn(e);
		}
	}

	/**
	 * Method addEventListener
	 * 
	 * @param event
	 *            String
	 * @param l
	 *            BackendEventListener
	 */
	public void addEventListener(final String event,
			final BackendEventListener l) {
		ArrayList<BackendEventListener> ls = fEventListeners.get(event);
		if (ls == null) {
			ls = new ArrayList<BackendEventListener>(20);
			fEventListeners.put(event, ls);
		}
		if (ls.indexOf(l) < 0) {
			ls.add(l);
		}
	}

	/**
	 * Method removeEventListener
	 * 
	 * @param event
	 *            String
	 * @param l
	 *            BackendEventListener
	 */
	public void removeEventListener(final String event,
			final BackendEventListener l) {
		final ArrayList<BackendEventListener> ls = fEventListeners.get(event);
		if (ls != null) {
			ls.remove(l);
		}
	}

	/**
	 * This executes in the event thread
	 */
	protected void handleReceiveEvent() {
		try {
			while (!Thread.currentThread().isInterrupted()) {
				OtpErlangObject msg = ftRpcBox.receive(1000);
				if (msg == null) {
					continue;
				}
				ErlLogger.debug("handleReceiveEvent() - Event! "
						+ msg.toString());
				// ErlUtils.match("stopped", msg)
				if (msg instanceof OtpErlangAtom) {
					final String sys = ((OtpErlangAtom) msg).atomValue();
					if (sys.compareTo("stopped") == 0) {
						break;
					}
					System.out
							.println("handleReceiveEvent() - Unrecognized system event: "
									+ sys);
				} else {
					final OtpErlangTuple t = (OtpErlangTuple) msg;
					msg = t.elementAt(1);
					String event = null;
					if (msg instanceof OtpErlangAtom) {
						event = ((OtpErlangAtom) msg).atomValue();
					} else if (msg instanceof OtpErlangTuple) {
						event = ((OtpErlangTuple) msg).elementAt(0).toString();
					}
					if (event != null) {
						final ArrayList<BackendEventListener> ls = fEventListeners
								.get(event);
						if (ls != null) {
							for (final BackendEventListener l : ls) {
								l.eventReceived(msg);
							}
						}
					}
				}
			}
			// ErlLogger.debug("exited event thread");
		} catch (final OtpErlangExit e) {
			ErlLogger.warn("Erlide backend: event source crashed.\n"
					+ e.getMessage());
		} catch (final OtpErlangDecodeException e) {
			ErlLogger.warn(e);
		}
	}

	public CodeManager getCodeManager() {
		return fCodeManager;
	}

	private RpcResult sendRpc(final String module, final String fun,
			final int timeout, final String signature, Object... args0)
			throws RpcException {
		if (!fAvailable) {
			return RpcResult.error("not connected");
		}
		return RpcUtil.sendRpc(fNode, fPeer, module, fun, timeout, signature,
				args0);
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

	public OtpErlangObject receiveRpc(final long timeout) throws OtpErlangExit,
			OtpErlangDecodeException {
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
		boolean inited = ErlideBackend.init(this, BackendManager
				.buildNodeName(getJavaNodeName()));
		if (!inited) {
			setAvailable(false);
		}
		getRpcDaemon().start();
	}

	public List<BackendEventListener> getEventListeners(final String event) {
		return fEventListeners.get(event);
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

	public void initializeRuntime(ILaunch launch) {
		launcher.initializeRuntime(launch);
		fShellManager = new BackendShellManager(this);
	}

	public void setRemoteRex(final OtpErlangPid watchdog) {
		try {
			getEventBox().link(watchdog);
		} catch (final OtpErlangExit e) {
		}
	}

	public void connectAndRegister(final List<ICodeBundle> plugins) {
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
			rpcx("code", "get_path", "");
		} catch (Throwable e) {
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
				addPath(false/* prefs.getUsePathZ() */, outDir);
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
		launcher.stop();
	}

}
