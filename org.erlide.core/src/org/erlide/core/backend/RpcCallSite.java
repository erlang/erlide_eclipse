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

import org.erlide.core.backend.internal.RpcFutureImpl;
import org.erlide.core.backend.rpc.RpcResult;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface RpcCallSite {

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

    RpcFutureImpl async_call(final String m, final String f,
            final String signature, final Object... args)
            throws BackendException;

    void async_call_cb(final RpcCallback cb, final String m, final String f,
            final String signature, final Object... args)
            throws BackendException;

    void cast(final String m, final String f, final String signature,
            final Object... args) throws BackendException;

    OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws BackendException;

    /**
     * typed RPC with timeout, throws Exception
     * 
     * @throws ConversionException
     */
    OtpErlangObject call(final int timeout, final String m, final String f,
            final String signature, final Object... a) throws BackendException;

    OtpErlangObject call(final int timeout, final OtpErlangObject gleader,
            final String m, final String f, final String signature,
            final Object... a) throws BackendException;

}
