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
package org.erlide.jinterface.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IRpcCallSite {

    /**
     * typed RPC
     * 
     */
    RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a);

    /**
     * typed RPC with timeout
     * 
     * @throws ConversionException
     */
    RpcResult call_noexception(final int timeout, final String m,
            final String f, final String signature, final Object... args);

    IRpcFuture async_call(final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    void async_call_cb(final IRpcCallback cb, final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    void cast(final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    OtpErlangObject call(final int timeout, final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    OtpErlangObject call(final int timeout, final OtpErlangObject gleader,
            final String m, final String f, final String signature,
            final Object... a) throws RpcException;

    public abstract void async_call_result(final IRpcResultCallback cb,
            final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    void send(final OtpErlangPid pid, final Object msg);

    void send(final String name, final Object msg);

}
