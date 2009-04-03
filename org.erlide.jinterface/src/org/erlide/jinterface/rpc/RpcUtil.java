/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import org.erlide.jinterface.JInterfaceFactory;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

public class RpcUtil {
	public static final int INFINITY = -1;

	private static final boolean VERBOSE = false;

	// use this for debugging
	private static final boolean CHECK_RPC = Boolean
			.getBoolean("org.erlide.checkrpc");

	/**
	 * Convenience method to send a remote message.
	 * 
	 * @param node
	 * @param pid
	 * @param msg
	 * @throws RpcException
	 */
	public static void send(OtpNode node, OtpErlangPid pid, Object msg)
			throws RpcException {
		final OtpMbox mbox = node.createMbox();
		try {
			if (mbox != null) {
				if (CHECK_RPC) {
					debug("SEND :: " + pid + " " + msg);
				}
				mbox.send(pid, RpcConverter.java2erlang(msg, "x"));
			}
		} finally {
			node.closeMbox(mbox);
		}
	}

	/**
	 * Convenience method to send a remote message.
	 * 
	 * @param node
	 * @param peer
	 * @param name
	 * @param msg
	 * @throws RpcException
	 */
	public static void send(final OtpNode node, String peer, String name,
			final Object msg) throws RpcException {
		final OtpMbox mbox = node.createMbox();
		try {
			if (mbox != null) {
				if (CHECK_RPC) {
					debug("SEND :: " + name + " " + msg);
				}
				mbox.send(name, peer, RpcConverter.java2erlang(msg, "x"));
			}
		} finally {
			node.closeMbox(mbox);
		}
	}

	/**
	 * Make a regular RPC to the given node, with the given arguments.
	 * 
	 * @param node
	 * @param peer
	 * @param module
	 * @param fun
	 * @param timeout
	 * @param signature
	 * @param args0
	 * @return
	 * @throws RpcException
	 */
	public static OtpErlangObject rpcCall(final OtpNode node, String peer,
			final String module, final String fun, final int timeout,
			final String signature, Object... args0) throws RpcException {
		RpcFuture future = sendRpcCall(node, peer, module, fun, signature,
				args0);
		OtpErlangObject result;
		result = future.get(timeout);
		return result;
	}

	/**
	 * Calls a function that supports sending progress reports back. The first
	 * argument is implicit and is the pid where the reports are to be sent.
	 */
	public static void rpcCallWithProgress(RpcResultCallback callback,
			final OtpNode node, String peer, final String module,
			final String fun, final int timeout, final String signature,
			Object... args0) throws RpcException {
		Object[] args = new Object[args0.length + 1];
		System.arraycopy(args0, 0, args, 1, args0.length);
		OtpMbox mbox = node.createMbox();
		args[0] = mbox.self();
		new RpcResultReceiver(mbox, callback);
		rpcCast(node, peer, module, fun, signature, args);
	}

	/**
	 * Send a RPC request and return the mailbox that will receive the result
	 * once it's delivered.
	 * 
	 * @param node
	 * @param peer
	 * @param module
	 * @param fun
	 * @param signature
	 * @param args0
	 * @return
	 * @throws RpcException
	 */
	public static RpcFuture sendRpcCall(final OtpNode node, String peer,
			final String module, final String fun, final String signature,
			Object... args0) throws RpcException {
		final OtpErlangObject[] args = convertArgs(signature, args0);

		OtpErlangObject res = null;
		final OtpMbox mbox = node.createMbox();
		res = RpcUtil.buildRpcCall(mbox.self(), module, fun, args);
		mbox.send("rex", peer, res);
		if (CHECK_RPC) {
			debug("RPC call:: " + res);
		}
		return new RpcFuture(mbox);
	}

	/**
	 * Retrieve the result of a RPC.
	 * 
	 * @param mbox
	 * @return
	 * @throws RpcException
	 */
	public static OtpErlangObject getRpcResult(OtpMbox mbox)
			throws RpcException {
		return getRpcResult(mbox, INFINITY);
	}

	/**
	 * Retrieve the result of a RPC.
	 * 
	 * @param mbox
	 * @param timeout
	 * @return
	 * @throws RpcException
	 */
	public static OtpErlangObject getRpcResult(OtpMbox mbox, long timeout)
			throws RpcException {
		assert mbox != null;

		OtpErlangObject res = null;
		try {
			try {
				if (timeout == INFINITY) {
					res = mbox.receive();
				} else {
					res = mbox.receive(timeout);
				}
				if (CHECK_RPC) {
					debug("    <= " + res);
				}
			} finally {
				mbox.close();
			}
			if (res == null) {
				throw new RpcTimeoutException("");
			}
			if (!(res instanceof OtpErlangTuple)) {
				throw new RpcException(res.toString());
			}
			res = ((OtpErlangTuple) res).elementAt(1);
		} catch (final OtpErlangExit e) {
			throw new RpcException(e);
		} catch (final OtpErlangDecodeException e) {
			throw new RpcException(e);
		}
		return res;
	}

	private static OtpErlangObject buildRpcCall(final OtpErlangPid pid,
			final String module, final String fun, final OtpErlangObject[] args) {
		final OtpErlangObject m = new OtpErlangAtom(module);
		final OtpErlangObject f = new OtpErlangAtom(fun);
		final OtpErlangObject a = new OtpErlangList(args);
		return JInterfaceFactory.mkTuple(pid, JInterfaceFactory.mkTuple(
				new OtpErlangAtom("call"), m, f, a, new OtpErlangAtom("user")));
	}

	/**
	 * Make a RPC but don't wait for any result.
	 * 
	 * @param node
	 * @param peer
	 * @param module
	 * @param fun
	 * @param signature
	 * @param args0
	 * @throws RpcException
	 */
	public static void rpcCast(final OtpNode node, String peer,
			final String module, final String fun, final String signature,
			Object... args0) throws RpcException {
		final OtpErlangObject[] args = convertArgs(signature, args0);

		OtpErlangObject res = null;
		res = RpcUtil.buildRpcCastMsg(module, fun, args);
		RpcUtil.send(node, peer, "rex", res);
		if (CHECK_RPC) {
			debug("RPC cast:: " + res);
		}
	}

	private static OtpErlangObject[] convertArgs(final String signature,
			Object... args0) throws RpcException {
		if (args0 == null) {
			args0 = new OtpErlangObject[] {};
		}

		Signature[] type = Signature.parse(signature);
		if (type == null) {
			type = new Signature[args0.length];
			for (int i = 0; i < args0.length; i++) {
				type[i] = new Signature('x');
			}
		}
		if (type.length != args0.length) {
			throw new RpcException("Signature doesn't match parameter number: "
					+ type.length + "/" + args0.length);
		}
		final OtpErlangObject[] args = new OtpErlangObject[args0.length];
		for (int i = 0; i < args.length; i++) {
			args[i] = RpcConverter.java2erlang(args0[i], type[i]);
		}
		return args;
	}

	private static OtpErlangObject buildRpcCastMsg(final String module,
			final String fun, final OtpErlangObject[] args) {
		final OtpErlangObject m = new OtpErlangAtom(module);
		final OtpErlangObject f = new OtpErlangAtom(fun);
		final OtpErlangObject a = new OtpErlangList(args);
		final OtpErlangAtom castTag = new OtpErlangAtom("$gen_cast");
		return JInterfaceFactory.mkTuple(castTag, JInterfaceFactory.mkTuple(
				new OtpErlangAtom("cast"), m, f, a, new OtpErlangAtom("user")));
	}

	private static void log(String s) {
		System.out.println("RpcUtil: " + s);
	}

	private static void debug(String s) {
		if (VERBOSE) {
			log(s);
		}
	}

	@SuppressWarnings("unused")
	private static void warn(Exception e) {
		log(e.getMessage());
		e.printStackTrace();
	}

	private RpcUtil() {
	}
}
