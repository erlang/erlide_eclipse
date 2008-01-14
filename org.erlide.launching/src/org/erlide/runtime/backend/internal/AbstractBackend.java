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
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.jinterface.rpc.RpcConverter;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.Cookie;
import org.erlide.runtime.backend.ErlRpcDaemon;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.ICodeManager;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangParseException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpPeer;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public abstract class AbstractBackend implements IBackend {

	// use this for debugging
	private static final boolean CHECK_RPC = false;

	final private HashMap<String, ArrayList<IBackendEventListener>> fEventListeners;

	private ICodeManager fCodeManager;

	private boolean fConnected = false;

	private class ThreadLocalMbox extends ThreadLocal<Object> {

		@Override
		protected Object initialValue() {
			final Object res = fNode.createMbox();
			if (res == null) {
				final String[] ss = fNode.getNames();
				StringBuffer sb = new StringBuffer();
				for (String element : ss) {
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

	protected OtpPeer fPeer;

	protected BackendShellManager fShellManager;

	private ErlRpcDaemon fRpcDaemon;

	private String fCurrentVersion;

	public AbstractBackend() {
		fLabel = null;

		fEventListeners = new HashMap<String, ArrayList<IBackendEventListener>>(
				10);
		fCodeManager = new CodeManager(this);
		fShellManager = new BackendShellManager(this);

	}

	public abstract void addProject(String project);

	public abstract void connect();

	public boolean ping() {
		return fNode.ping(fPeer.node(), 500);
	}

	protected void doConnect(String label) {
		ErlLogger.debug(">>:: " + label);
		try {
			wait_for_epmd();

			fNode = new OtpNode(BackendManager
					.buildNodeName(BackendManager.JAVA_NODE_LABEL), Cookie
					.retrieveCookie());
			ErlLogger.debug("java node is " + fNode.node());

			fPeer = new OtpPeer(BackendManager.buildNodeName(label));
			ErlLogger.debug("erlang peer is " + label + " ("
					+ BackendManager.buildNodeName(label) + ")-- "
					+ fPeer.node());

			int tries = Integer.parseInt(System.getProperty(
					"erlide.backend.retries", "50"));
			boolean conn = false;
			while (!conn && tries > 0) {
				System.out.print("#");
				conn = fNode.ping(fPeer.node(), 500);
				tries--;
			}
			ftMBox = new ThreadLocalMbox();
			ftRpcBox = fNode.createMbox("rex");
			if (tries > 0) {
				ErlLogger.debug("connected to peer!");
			} else {
				ErlLogger.debug("Couldn't connect!!!");
			}
			fConnected = tries > 0;

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

		fShellManager.dispose();
		fRpcDaemon.stop();
	}

	public RpcResult rpc(String m, String f, Object... a)
			throws ErlangRpcException {
		return rpct(m, f, 5000, a);
	}

	/**
	 * RPC with timeout
	 */
	public RpcResult rpct(String m, String f, int timeout, Object... a)
			throws ErlangRpcException {
		return sendRpc(m, f, timeout, a);
	}

	/**
	 * RPC , throws Exception
	 */
	public OtpErlangObject rpcx(String m, String f, Object... a)
			throws ErlangRpcException, BackendException {
		return rpcxt(m, f, 5000, a);
	}

	/**
	 * RPC with timeout, throws Exception
	 */
	public OtpErlangObject rpcxt(String m, String f, int timeout, Object... a)
			throws ErlangRpcException, BackendException {
		return checkRpc(rpct(m, f, timeout, a));
	}

	private static OtpErlangObject checkRpc(RpcResult r)
			throws BackendException {
		if (r != null && r.isOk()) {
			return r.getValue();
		}
		if (r == null) {
			throw new BackendException("RPC error: null response (timeout?)");
		}
		throw new BackendException("RPC error: " + r.getValue());
	}

	/**
	 * 
	 * @param dbgPid
	 * @param msg
	 */
	public void send(OtpErlangPid pid, Object msg) {
		getMbox().send(pid, RpcConverter.java2erlang(msg));
	}

	public void send(String name, Object msg) {
		getMbox().send(name, fPeer.node(), RpcConverter.java2erlang(msg));
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
					String sys = ((OtpErlangAtom) msg).atomValue();
					if (sys.compareTo("stopped") == 0) {
						break;
					} else {
						System.out
								.println("handleReceiveEvent() - Unrecognized system event: "
										+ sys);
					}
				} else {
					OtpErlangTuple t = (OtpErlangTuple) msg;
					msg = t.elementAt(1);
					String event = null;
					if (msg instanceof OtpErlangAtom) {
						event = ((OtpErlangAtom) msg).atomValue();
					} else if (msg instanceof OtpErlangTuple) {
						event = ((OtpErlangTuple) msg).elementAt(0).toString();
					}
					if (event != null) {
						ArrayList<IBackendEventListener> ls = fEventListeners
								.get(event);
						if (ls != null) {
							for (IBackendEventListener l : ls) {
								l.eventReceived(msg);
							}
						}
					}
				}
			}
			// ErlLogger.debug("exited event thread");
		} catch (OtpErlangExit e) {
			ErlLogger.debug("Erlide backend: event source crashed.\n"
					+ e.getMessage());
		} catch (OtpErlangDecodeException e) {
			e.printStackTrace();
		}
	}

	public ICodeManager getCodeManager() {
		return fCodeManager;
	}

	private RpcResult sendRpc(String module, String fun, int timeout,
			Object... args0) {
		if (!fConnected) {
			return null;
		}
		if (args0 == null) {
			args0 = new OtpErlangObject[] {};
		}
		OtpErlangObject[] args = null;
		args = new OtpErlangObject[args0.length];
		for (int i = 0; i < args.length; i++) {
			args[i] = RpcConverter.java2erlang(args0[i]);
		}

		RpcResult result = null;
		OtpErlangObject res = null;
		try {
			final OtpMbox mbox = getMbox();
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
			// res = erlang2java(res); // ??
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
		return ftMBox.getMbox();
	}

	private OtpMbox getEventBox() {
		return ftRpcBox;
	}

	public OtpErlangPid getEventPid() {
		return getEventBox().self();
	}

	public OtpErlangPid getRpcPid() {
		return getMbox().self();
	}

	public String getCurrentVersion() {
		if (fCurrentVersion == null) {
			fCurrentVersion = "";
			OtpErlangObject r;
			try {
				r = rpcx("init", "script_id");
				if (r instanceof OtpErlangTuple) {
					OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
					if (rr instanceof OtpErlangString) {
						fCurrentVersion = ((OtpErlangString) rr).stringValue();
					}
				}
			} catch (ErlangRpcException e) {
			} catch (BackendException e) {
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
			ErlLogger
					.error("Couldn't contact epmd - erlang backend is probably not working\n"
							+ "  Possibly your host's entry in /etc/hosts is wrong.");
			throw new BackendException(
					"Couldn't contact epmd - erlang backend is probably not working\n"
							+ "  Possibly your host's entry in /etc/hosts is wrong.");
		}
	}

	public OtpErlangObject receive(int timeout) throws OtpErlangExit,
			OtpErlangDecodeException {
		return getMbox().receive(timeout);
	}

	public OtpErlangObject receiveEvent() throws OtpErlangExit,
			OtpErlangDecodeException {
		OtpMbox eventBox = getEventBox();
		if (eventBox != null) {
			return eventBox.receive();
		} else {
			return null;
		}
	}

	public OtpErlangObject receiveRpc(long timeout) throws OtpErlangExit,
			OtpErlangDecodeException {
		OtpMbox eventBox = getEventBox();
		if (eventBox != null) {
			return eventBox.receive(timeout);
		} else {
			return null;
		}
	}

	public OtpErlangObject execute(String fun, OtpErlangObject... args)
			throws ErlangRpcException {
		return rpc(ERL_BACKEND, "execute", new OtpErlangString(fun),
				new OtpErlangList(args)).getValue();
	}

	public BackendShellManager getShellManager() {
		return fShellManager;
	}

	public abstract void sendToDefaultShell(String msg) throws IOException;

	public abstract void sendToShell(String str);

	public abstract void addStdListener(IStreamListener dsp);

	public abstract boolean isManaged();

	public String getLabel() {
		return fLabel;
	}

	public void setLabel(String lbl) {
		fLabel = lbl;
	}

	public abstract ILaunch initialize();

	public void init_erlang() {
		fRpcDaemon = new ErlRpcDaemon(this);
		try {
			rpc(ERL_BACKEND, "init", new OtpErlangAtom(fNode.node()));
		} catch (ErlangRpcException e) {
			e.printStackTrace();
		}
	}

	public List<IBackendEventListener> getEventListeners(String event) {
		return fEventListeners.get(event);
	}

	/**
	 * @param string
	 * @return OtpErlangobject
	 * @throws ErlangParseException
	 */
	@SuppressWarnings("unused")
	private OtpErlangObject parseTerm(String string)
			throws ErlangParseException {
		OtpErlangObject r = null;
		try {
			r = rpcx(ERL_BACKEND, "parse_term", new OtpErlangString(string));
			ErlLogger.debug("PARSE=" + r);
		} catch (final Exception e) {
			e.printStackTrace();
			throw new ErlangParseException("Could not parse term \"" + string
					+ "\"");
		}
		return r;
	}

}
