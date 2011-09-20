/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.core.backend;

import org.erlide.core.rpc.IRpcCallback;
import org.erlide.core.rpc.IRpcFuture;
import org.erlide.core.rpc.IRpcResultCallback;
import org.erlide.core.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.SignatureException;

public interface IErlRuntime {

    String getNodeName();

    boolean connect();

    void remoteStatus(final String node, final boolean up, final Object info);

    void makeAsyncResultCall(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object[] args)
            throws SignatureException;

    IRpcFuture makeAsyncCall(final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException, SignatureException;

    IRpcFuture makeAsyncCall(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException,
            SignatureException;

    void makeAsyncCbCall(final IRpcCallback cb, final int timeout,
            final String module, final String fun, final String signature,
            final Object... args) throws RpcException, SignatureException;

    void makeAsyncCbCall(final IRpcCallback cb, final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args)
            throws RpcException, SignatureException;

    OtpErlangObject makeCall(final int timeout, final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException, SignatureException;

    OtpErlangObject makeCall(final int timeout, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException, SignatureException;

    void makeCast(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException, RpcException;

    void makeCast(final String module, final String fun,
            final String signature, final Object... args0)
            throws SignatureException, RpcException;

    boolean isAvailable();

    OtpNode getNode();

    void send(OtpErlangPid pid, Object msg) throws SignatureException,
            RpcException;

    void send(String fullNodeName, String name, Object msg)
            throws SignatureException, RpcException;

}
