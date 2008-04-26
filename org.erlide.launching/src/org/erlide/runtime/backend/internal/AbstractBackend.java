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

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.jinterface.rpc.RpcConverter.Signature;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.IDisposable;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.Cookie;
import org.erlide.runtime.backend.ErlRpcDaemon;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.ICodeManager;
import org.erlide.runtime.backend.RpcResult;
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

import erlang.ErlideBackend;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public abstract class AbstractBackend implements IBackend, IDisposable {

	// use this for debugging
	private static final boolean CHECK_RPC = false;
	// private static final boolean TRACE = false;

	final private HashMap<String, ArrayList<IBackendEventListener>> fEventListeners;
	private final ICodeManager fCodeManager;
	boolean fConnected = false;

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

	private OtpMbox ftRpcBox; // ingoing rpc and events

	// private static final boolean TRACE = false;

	protected String fLabel;

	protected static String fHost;

	protected OtpNode fNode;

	protected String fPeer;

	protected IShellManager fShellManager;

	private String fCurrentVersion;

	public AbstractBackend() {
		fLabel = null;

		fEventListeners = new HashMap<String, ArrayList<IBackendEventListener>>(
				10);
		fCodeManager = new CodeManager(this);
		fShellManager = new BackendShellManager(this);
	}

	public void connect() {
		connect(null);
	}

	public abstract void connect(String cookie);

	public boolean ping() {
		return fNode.ping(fPeer, 500);
	}

	protected void doConnect(String label) {
		doConnect(label, null);
	}

	protected void doConnect(String label, String cookie) {
		ErlLogger.debug("connect to :: " + label);
		try {
			wait_for_epmd();

			if (cookie == null) {
				cookie = Cookie.retrieveCookie();
			}
			fNode = new OtpNode(BackendManager
					.buildNodeName(BackendManager.JAVA_NODE_LABEL), cookie);
			ErlLogger.debug("java node is " + fNode.node());

			fPeer = BackendManager.buildNodeName(label);
			ErlLogger.debug("erlang peer is " + label + " " + fPeer);

			ftMBox = new ThreadLocalMbox();
			ftRpcBox = fNode.createMbox("rex");

			int tries = Integer.parseInt(System.getProperty(
					"erlide.backend.retries", "50"));
			boolean conn = false;
			while (!conn && tries > 0) {
				ErlLogger.debug("trying to connect...");
				conn = fNode.ping(fPeer, 500);
				tries--;
			}
			if (tries > 0) {
				ErlLogger.debug("connected to peer!");
			} else {
				ErlLogger.error("Couldn't connect!!!");
			}
			synchronized (this) {
				fConnected = tries > 0;
				notifyAll();
			}

		} catch (final Exception e) {
			e.printStackTrace();
			ErlangLaunchPlugin.log(e);
			fConnected = false;
		}
	}

	/**
	 * Method dispose
	 */
	public void dispose() {
		ErlLogger.debug("disposing backend " + fLabel);

		if (fNode != null) {
			fNode.close();
		}

		if (fShellManager instanceof IDisposable) {
			((IDisposable) fShellManager).dispose();
		}
		ErlRpcDaemon.getInstance().stop();
	}

	/**
	 * typed RPC
	 * 
	 * @throws NoBackendException
	 * 
	 * @throws ConversionException
	 */
	public RpcResult rpc(String m, String f, String signature, Object... a)
			throws RpcException {
		return rpc(m, f, 5000, signature, a);
	}

	/**
	 * typed RPC with timeout
	 * 
	 * @throws ConversionException
	 */
	public RpcResult rpc(String m, String f, int timeout, String signature,
			Object... a) throws RpcException {
		return sendRpc(m, f, timeout, signature, a);
	}

	/**
	 * typed RPC , throws Exception
	 * 
	 * @throws BackendException
	 * 
	 * @throws ConversionException
	 */
	public OtpErlangObject rpcx(String m, String f, String signature,
			Object... a) throws RpcException, BackendException {
		return rpcx(m, f, 5000, signature, a);
	}

	/**
	 * typed RPC with timeout, throws Exception
	 * 
	 * @throws ConversionException
	 */
	public OtpErlangObject rpcx(String m, String f, int timeout,
			String signature, Object... a) throws BackendException,
			RpcException {
		return checkRpc(rpc(m, f, timeout, signature, a));
	}

	private static OtpErlangObject checkRpc(RpcResult r)
			throws BackendException, NoBackendException {
		if (r != null && r.isOk()) {
			return r.getValue();
		}
		if (r == null) {
			throw new NoBackendException();
		}
		throw new BackendException("RPC error: " + r.getValue());
	}

	/**
	 * 
	 * @param msg
	 * @param dbgPid
	 * @throws ConversionException
	 */
	public void send(OtpErlangPid pid, Object msg) {
		try {
			OtpMbox mbox = getMbox();
			if (mbox != null) {
				mbox.send(pid, RpcConverter.java2erlang(msg, "x"));
			}
		} catch (final RpcException e) {
			// shouldn't happen
			e.printStackTrace();
		}
	}

	public void send(String name, Object msg) {
		try {
			OtpMbox mbox = getMbox();
			if (mbox != null) {
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
	public void addEventListener(String event, IBackendEventListener l) {
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
	public void removeEventListener(String event, IBackendEventListener l) {
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

	public ICodeManager getCodeManager() {
		return fCodeManager;
	}

	private RpcResult sendRpc(String module, String fun, int timeout,
			String signature, Object... args0) throws RpcException {
		// synchronized (this) {
		// while (!fConnected) {
		// try {
		// wait();
		// } catch (InterruptedException e) {
		// e.printStackTrace();
		// }
		// }
		// }
		if (!fConnected) {
			return null;
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
				return null;
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
				ErlLogger.debug("    -> " + res);
			}

			if (res == null) {
				if (CHECK_RPC) {
					ErlLogger.debug("    timed out: " + module + ":" + fun
							+ "(" + new OtpErlangList(args) + ")");
				}
				return null;
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
		OtpMbox eventBox = getEventBox();
		if (eventBox == null) {
			return null;
		}
		return eventBox.self();
	}

	public OtpErlangPid getRpcPid() {
		OtpMbox mbox = getMbox();
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
			String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
					+ "  Possibly your host's entry in /etc/hosts is wrong.";
			ErlLogger.error(msg);
			throw new BackendException(msg);
		}
	}

	public OtpErlangObject receive(int timeout) throws OtpErlangExit,
			OtpErlangDecodeException {
		OtpMbox mbox = getMbox();
		if (mbox == null) {
			return null;
		}
		return mbox.receive(timeout);
	}

	public OtpErlangObject receiveRpc(long timeout) throws OtpErlangExit,
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

	public abstract void sendToShell(String str);

	public abstract void addStdListener(IStreamListener dsp);

	public String getLabel() {
		return fLabel;
	}

	public void setLabel(String lbl) {
		fLabel = lbl;
	}

	public abstract ILaunch initialize();

	public void init_erlang() {
		ErlRpcDaemon.getInstance().start(this);
		// ErlideBackend.init(this, fNode.node());
	}

	public List<IBackendEventListener> getEventListeners(String event) {
		return fEventListeners.get(event);
	}

	public String getName() {
		if (fNode == null) {
			return "<not connected>";
		}
		return fNode.node();
	}

	public String getHost() {
		// TODO fix Backend.getHost()
		return "localhost";
	}

}
