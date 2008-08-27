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
package org.erlide.runtime.backend.internal;

import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.jinterface.rpc.RpcConverter.Signature;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.IDisposable;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.runtime.backend.ErlRpcDaemon;
import org.erlide.runtime.backend.ExecutionBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.console.IShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;

import erlang.ErlideBackend;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public abstract class AbstractBackend extends OtpNodeStatus implements
		IdeBackend, BuildBackend, ExecutionBackend, IDisposable {

	// use this for debugging
	private static final boolean CHECK_RPC = "true".equals(System
			.getProperty("org.erlide.checkrpc"));

	final private HashMap<String, ArrayList<IBackendEventListener>> fEventListeners;
	private final CodeManager fCodeManager;
	boolean fAvailable = false;

	class ThreadLocalMbox extends ThreadLocal<Object> {

		@Override
		protected Object initialValue() {
			final Object res = fNode.createMbox();
			if (res == null) {
				final String[] ss = fNode.getNames();
				final StringBuilder sb = new StringBuilder();
				for (final String element : ss) {
					sb.append(element).append(" ");
				}
				ErlLogger.debug(sb.toString());
			}
			return res;
		}

		public OtpMbox getMbox() {
			final OtpMbox res = (OtpMbox) super.get();
			return res;
		}
	}

	private ThreadLocalMbox ftMBox; // outgoing rpc and send
	private OtpMbox ftRpcBox; // incoming rpc and events
	protected static String fHost;
	protected OtpNode fNode;
	protected String fPeer;
	protected IShellManager fShellManager;
	private String fCurrentVersion;
	private final RuntimeInfo fInfo;
	private boolean fDebug;
	private ErlRpcDaemon rpcDaemon;

	public AbstractBackend(final RuntimeInfo info) throws BackendException {
		if (info == null) {
			throw new BackendException(
					"Can't create backend without runtime information");
		}
		fEventListeners = new HashMap<String, ArrayList<IBackendEventListener>>();
		fCodeManager = new CodeManager(this);
		fShellManager = new BackendShellManager(this);
		fInfo = info;
	}

	public abstract void connect();

	public boolean ping() {
		return fNode.ping(fPeer, 500);
	}

	protected void doConnect(final String label) {
		ErlLogger.debug("connect to:: '" + label + "' "
				+ Thread.currentThread());
		// Thread.dumpStack();
		try {
			wait_for_epmd();

			String cookie = getInfo().getCookie();
			fNode = new OtpNode(BackendManager.getDefault().getJavaNodeName(),
					cookie);
			fNode.registerStatusHandler(this);
			fPeer = BackendManager.buildNodeName(label);
			ErlLogger.debug("java node is " + fNode.node()
					+ "; erlang peer is " + fPeer);

			ftMBox = new ThreadLocalMbox();
			ftRpcBox = fNode.createMbox("rex");
			int tries = 50;
			while (!fAvailable && tries > 0) {
				fAvailable = fNode.ping(fPeer, 200);
				tries--;
			}
			ErlLogger.debug("connected!");

		} catch (final Exception e) {
			e.printStackTrace();
			ErlangLaunchPlugin.log(e);
			fAvailable = false;
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
		getRpcDaemon().stop();
	}

	private ErlRpcDaemon getRpcDaemon() {
		if (rpcDaemon == null) {
			rpcDaemon = new ErlRpcDaemon(this);
		}
		return rpcDaemon;
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
			sa.append("'").append(x.toString()).append("',");
		}
		final String ss = sa.toString().replaceAll("[\\r\\n]", " ");
		final String msg = String.format("%s <- %s:%s(%s)", r.getValue(), m, f,
				ss);
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
			final OtpMbox mbox = getMbox();
			if (mbox != null) {
				if (CHECK_RPC) {
					ErlLogger.debug("SEND :: " + pid + " " + msg);
				}
				mbox.send(pid, RpcConverter.java2erlang(msg, "x"));
			}
		} catch (final RpcException e) {
			// shouldn't happen
			e.printStackTrace();
		}
	}

	public void send(final String name, final Object msg) {
		try {
			final OtpMbox mbox = getMbox();
			if (mbox != null) {
				if (CHECK_RPC) {
					ErlLogger.debug("SEND :: " + name + " " + msg);
				}
				mbox.send(name, fPeer, RpcConverter.java2erlang(msg, "x"));
			}
		} catch (final RpcException e) {
			// shouldn't happen
			e.printStackTrace();
		}
	}

	/**
	 * Method addEventListener
	 * 
	 * @param event
	 *            String
	 * @param l
	 *            IBackendEventListener
	 */
	public void addEventListener(final String event,
			final IBackendEventListener l) {
		ArrayList<IBackendEventListener> ls = fEventListeners.get(event);
		if (ls == null) {
			ls = new ArrayList<IBackendEventListener>(20);
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
	 *            IBackendEventListener
	 */
	public void removeEventListener(final String event,
			final IBackendEventListener l) {
		final ArrayList<IBackendEventListener> ls = fEventListeners.get(event);
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
						final ArrayList<IBackendEventListener> ls = fEventListeners
								.get(event);
						if (ls != null) {
							for (final IBackendEventListener l : ls) {
								l.eventReceived(msg);
							}
						}
					}
				}
			}
			// ErlLogger.debug("exited event thread");
		} catch (final OtpErlangExit e) {
			ErlLogger.debug("Erlide backend: event source crashed.\n"
					+ e.getMessage());
		} catch (final OtpErlangDecodeException e) {
			e.printStackTrace();
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
		if (args0 == null) {
			args0 = new OtpErlangObject[] {};
		}

		Signature[] type = RpcConverter.parseSignature(signature);
		if (type == null) {
			type = new Signature[args0.length];
			for (int i = 0; i < type.length; i++) {
				type[i] = new Signature('x');
			}
		}
		if (type.length != args0.length) {
			throw new RpcException("Signature doesn't match parameter number");
		}
		final OtpErlangObject[] args = new OtpErlangObject[args0.length];
		for (int i = 0; i < args.length; i++) {
			args[i] = RpcConverter.java2erlang(args0[i], type[i]);
		}

		RpcResult result = null;
		OtpErlangObject res = null;
		try {
			final OtpMbox mbox = getMbox();
			if (mbox == null) {
				return RpcResult.error("missing receive mailbox");
			}
			res = RpcUtil.buildRpcCall(module, fun, args, mbox.self());
			send("rex", res);
			if (CHECK_RPC) {
				ErlLogger.debug("RPC :: " + res);
			}

			if (timeout < 0) {
				res = getMbox().receive();
			} else {
				res = getMbox().receive(timeout);
			}
			if (CHECK_RPC) {
				ErlLogger.debug("    <= " + res);
			}

			if (res == null) {
				if (CHECK_RPC) {
					ErlLogger.debug("    timed out: " + module + ":" + fun
							+ "(" + new OtpErlangList(args) + ")");
				}
				return RpcResult.error("timeout");
			}

			res = ((OtpErlangTuple) res).elementAt(1);
			result = new RpcResult(res);
		} catch (final OtpErlangExit e) {
			e.printStackTrace();
			ErlangLaunchPlugin.log(e);
		} catch (final OtpErlangDecodeException e) {
			e.printStackTrace();
			ErlangLaunchPlugin.log(e);
		}
		return result;
	}

	private OtpMbox getMbox() {
		if (ftMBox == null) {
			return null;
		}
		return ftMBox.getMbox();
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

	public OtpErlangPid getRpcPid() {
		final OtpMbox mbox = getMbox();
		if (mbox == null) {
			return new OtpErlangPid("", 0, 0, 0);
		}
		return mbox.self();
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

	public OtpErlangObject receive(final int timeout) throws OtpErlangExit,
			OtpErlangDecodeException {
		final OtpMbox mbox = getMbox();
		if (mbox == null) {
			return null;
		}
		return mbox.receive(timeout);
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

	public abstract void sendToDefaultShell(String msg) throws IOException;

	public abstract void addStdListener(IStreamListener dsp);

	public void initErlang() {
		ErlideBackend.init(this, BackendManager
				.buildNodeName(getJavaNodeName()));
		getRpcDaemon().start();
	}

	public List<IBackendEventListener> getEventListeners(final String event) {
		return fEventListeners.get(event);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.runtime.backend.IBackend#getInfo()
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

	public abstract void initializeRuntime(ILaunch launch);

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
	}

	@Override
	public void remoteStatus(final String node, final boolean up,
			final Object info) {
		// final String dir = up ? "up" : "down";
		// ErlLogger.debug(String.format("@@: %s %s %s", node, dir, info));
		if (node.equals(fPeer)) {
			setAvailable(up);
		}
	}

	private void setAvailable(final boolean up) {
		// TODO notify others? BackendManager?
		fAvailable = up;
	}

	@Override
	public void connAttempt(final String node, final boolean incoming,
			final Object info) {
		final String direction = incoming ? "incoming" : "outgoing";
		ErlLogger.debug(String.format("@@: Connection attempt: %s %s %s", node,
				direction, info));
	}

	public IdeBackend asIDE() {
		return this;
	}

	public BuildBackend asBuild() {
		return this;
	}

	public ExecutionBackend asExecution() {
		return this;
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

	public void registerProjects(String[] projectNames) {
		for (String s : projectNames) {
			IProject project = ResourcesPlugin.getWorkspace().getRoot()
					.getProject(s);
			final ErlangProjectProperties prefs = new ErlangProjectProperties(
					project);
			final String outDir = project.getLocation().append(
					prefs.getOutputDir()).toOSString();
			if (outDir.length() > 0) {
				addPath(prefs.getUsePathZ(), outDir);
			}
		}

	}
}
