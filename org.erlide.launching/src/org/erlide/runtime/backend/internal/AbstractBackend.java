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
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.Cookie;
import org.erlide.runtime.backend.ErlRpcDaemon;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.IBackendEventListener;
import org.erlide.runtime.backend.ICodeManager;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.console.BackendShellManager;
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
 * @author Vlad Dumitrescu [vlad_dumitrescu at hotmail dot com]
 */
public abstract class AbstractBackend implements IBackend {

	// use this for debugging
	private static final boolean CHECK_RPC = false;

	final private HashMap<String, ArrayList<IBackendEventListener>> fEventListeners;

	private ICodeManager fCodeManager;

	private boolean fConnected = false;

	private class ThreadLocalMbox extends ThreadLocal {

		@Override
		protected Object initialValue() {
			final Object res = fNode.createMbox();
			if (res == null) {
				final String[] ss = fNode.getNames();
				StringBuffer sb = new StringBuffer();
				for (String element : ss) {
					sb.append(element).append(" ");
				}
				ErlLogger.log(sb);
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
		ErlLogger.log(">>:: " + label);
		try {
			wait_for_epmd();

			fNode = new OtpNode(BackendManager
					.buildNodeName(BackendManager.JAVA_NODE_LABEL), Cookie
					.retrieveCookie());
			ErlLogger.log("java node is " + fNode.node());

			fPeer = new OtpPeer(BackendManager.buildNodeName(label));
			ErlLogger.log("erlang peer is " + label + " ("
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
				ErlLogger.log("connected to peer!");
			} else {
				ErlLogger.log("Couldn't connect!!!");
			}
			fConnected = tries > 0;

		} catch (final IOException e) {
			e.printStackTrace();
			ErlangLaunchPlugin.log(e);
			fConnected = false;
		}
	}

	/**
	 * Method dispose
	 */
	public void dispose() {
		ErlLogger.log("disposing backend " + fLabel);

		if (fNode != null) {
			fNode.close();
		}

		fShellManager.dispose();
		fRpcDaemon.stop();
	}

	/**
	 * @throws ErlangRpcException
	 * 
	 * @param m
	 * @param f
	 * @return OtpErlangObject
	 */
	public RpcResult rpc(String m, String f) throws ErlangRpcException {
		return rpc(m, f, new OtpErlangObject[] {});
	}

	/**
	 * @throws ErlangRpcException
	 * 
	 * @param m
	 * @param f
	 * @param a1
	 * @return OtpErlangObject
	 */
	public RpcResult rpc(String m, String f, OtpErlangObject... a)
			throws ErlangRpcException {
		return rpc(m, f, a, 10000);
	}

	/**
	 * @throws ErlangRpcException
	 * @param m
	 * @param f
	 * @param a
	 * @return OtpErlangObject
	 * @throws ErlangParseException
	 */
	public RpcResult rpc(String m, String f, String[] a)
			throws ErlangParseException, ErlangRpcException {
		final OtpErlangObject[] args = new OtpErlangObject[a.length];

		for (int i = 0; i < a.length; i++) {
			args[i] = parseTerm(a[i]);
		}

		return rpc(m, f, args, 2000);
	}

	/**
	 * @param string
	 * @return OtpErlangobject
	 * @throws ErlangParseException
	 */
	private OtpErlangObject parseTerm(String string)
			throws ErlangParseException {
		OtpErlangObject r = null;
		try {
			r = BackendUtil.checkRpc(rpc(ERL_BACKEND, "parse_term",
					new OtpErlangString(string)));
			ErlLogger.log("PARSE=" + r);
		} catch (final Exception e) {
			e.printStackTrace();
			throw new ErlangParseException("Could not parse term \"" + string
					+ "\"");
		}
		return r;
	}

	/**
	 */
	public RpcResult rpc(String m, String f, OtpErlangObject[] a, int timeout)
			throws ErlangRpcException {
		return sendRpc(m, f, a, timeout);
	}

	/**
	 * 
	 * @param dbgPid
	 * @param msg
	 */
	public void send(OtpErlangPid pid, OtpErlangObject msg) {
		getMbox().send(pid, msg);
	}

	public void send(String name, OtpErlangObject msg) {
		getMbox().send(name, fPeer.node(), msg);
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
				if (msg == null)
					continue;
				ErlLogger
						.log("handleReceiveEvent() - Event! " + msg.toString());
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
						ArrayList ls = fEventListeners.get(event);
						if (ls != null) {
							for (int i = 0; i < ls.size(); i++) {
								((IBackendEventListener) ls.get(i))
										.eventReceived(msg);
							}
						}
					}
				}
			}
			// ErlLogger.log("exited event thread");
		} catch (OtpErlangExit e) {
			ErlLogger.log("Erlide backend: event source crashed.\n"
					+ e.getMessage());
		} catch (OtpErlangDecodeException e) {
			e.printStackTrace();
		}
	}

	public ICodeManager getCodeManager() {
		return fCodeManager;
	}

	private RpcResult sendRpc(String module, String fun,
			OtpErlangObject[] args, int timeout) {
		if (!fConnected) {
			return null;
		}

		RpcResult result = null;
		OtpErlangObject res = null;
		try {
			final OtpMbox mbox = getMbox();
			res = buildRpcCall(module, fun, args, mbox.self());
			send("rex", res);
			if (CHECK_RPC) {
				ErlLogger.log("RPC :: " + res);
			}

			if (timeout < 0) {
				res = getMbox().receive();
			} else {
				res = getMbox().receive(timeout);
			}
			if (CHECK_RPC) {
				ErlLogger.log("    -> " + res);
			}

			if (res == null) {
				if (CHECK_RPC) {
					ErlLogger.log("    timed out: " + module + ":" + fun + "("
							+ new OtpErlangList(args) + ")");
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

	private OtpErlangTuple buildRpcCall(final String module, final String fun,
			final OtpErlangObject[] args, final OtpErlangPid pid) {
		final OtpErlangObject m = new OtpErlangAtom(module);
		final OtpErlangObject f = new OtpErlangAtom(fun);
		final OtpErlangObject a = new OtpErlangList(args);
		return new OtpErlangTuple(new OtpErlangObject[] {
				pid,
				new OtpErlangTuple(new OtpErlangObject[] {
						new OtpErlangAtom("call"), m, f, a,
						new OtpErlangAtom("user") }) });
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
		try {
			final OtpErlangObject r = BackendUtil.checkRpc(rpc("init",
					"script_id"));
			final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
			return ((OtpErlangString) rr).stringValue();
		} catch (final Exception e) {
			return "0";
		}
	}

	private static final int EPMD_PORT = 4369;

	protected void wait_for_epmd() {
		// If anyone has a better solution for waiting for epmd to be up, please
		// let me know
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
				// ErlLogger.log("sleep............");
			} catch (final InterruptedException e1) {
			}

		} while (!ok);
	}

	public OtpErlangObject receive(int i) throws OtpErlangExit,
			OtpErlangDecodeException {
		return getMbox().receive(i);
	}

	public OtpErlangObject receiveEvent() throws OtpErlangExit,
			OtpErlangDecodeException {
		return getEventBox().receive();
	}

	public OtpErlangObject receiveRpc(long timeout) throws OtpErlangExit,
			OtpErlangDecodeException {
		return getEventBox().receive(timeout);
	}

	public OtpErlangObject execute(String fun, OtpErlangObject[] args)
			throws ErlangRpcException {
		final OtpErlangObject[] allargs = new OtpErlangObject[2];
		allargs[0] = new OtpErlangString(fun);
		allargs[1] = new OtpErlangList(args);
		return rpc(ERL_BACKEND, "execute", allargs).getValue();
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

	public ErlRpcDaemon getRpcDaemon() {
		return fRpcDaemon;
	}

	public List<IBackendEventListener> getEventListeners(String event) {
		return fEventListeners.get(event);
	}
}
