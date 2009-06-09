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
import com.ericsson.otp.erlang.SignatureException;

public class Backend extends BackendNode {

	private static int DEFAULT_TIMEOUT;
	{
		final String t = System.getProperty("erlide.rpc.timeout", "9000");
		if ("infinity".equals(t)) {
			DEFAULT_TIMEOUT = RpcUtil.INFINITY;
		} else {
			DEFAULT_TIMEOUT = Integer.parseInt(t);
		}
	}
	private IShellManager shellManager;

	public IShellManager getShellManager() {
		return shellManager;
	}

	@Override
	public void initializeRuntime() {
		super.initializeRuntime();
		shellManager = new BackendShellManager(this);
	}

	protected Backend(final RuntimeInfo info) throws BackendException {
		super(info);
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

	public OtpErlangObject receiveEvent(final long timeout)
			throws OtpErlangExit, OtpErlangDecodeException {
		if (eventBox == null) {
			return null;
		}
		return eventBox.receive(timeout);
	}

	@Override
	public void dispose() {
		if (shellManager instanceof IDisposable) {
			((IDisposable) shellManager).dispose();
		}
		super.dispose();
	}

}
