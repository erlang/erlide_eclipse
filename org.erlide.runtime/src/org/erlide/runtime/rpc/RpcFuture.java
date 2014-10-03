/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.rpc;

import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.internal.rpc.OtpRpc;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.util.concurrent.CheckedFuture;

public class RpcFuture implements CheckedFuture<OtpErlangObject, RpcException> {

    private final OtpMbox mbox;
    private OtpErlangObject result = null;
    private final String env;
    private final boolean logCalls;

    private final IOtpRpc rpc;
    private final OtpErlangRef ref;

    public RpcFuture(final OtpErlangRef ref, final OtpMbox mbox, final String env,
            final boolean logCalls, final IOtpRpc rpc) {
        this.ref = ref;

        this.mbox = mbox;
        this.env = env;
        this.logCalls = logCalls;
        this.rpc = rpc;
    }

    @Override
    public OtpErlangObject get() {
        try {
            return checkedGet();
        } catch (final RpcException e) {
            return null;
        }
    }

    @Override
    public OtpErlangObject get(final long timeout, final TimeUnit unit) {
        try {
            return checkedGet(timeout, unit);
        } catch (final TimeoutException e) {
            return null;
        } catch (final RpcException e) {
            return null;
        }
    }

    @Override
    public boolean isDone() {
        return result != null;
    }

    @Override
    public void addListener(final Runnable listener, final Executor executor) {
    }

    @Override
    public boolean cancel(final boolean mayInterruptIfRunning) {
        return false;
    }

    @Override
    public boolean isCancelled() {
        return false;
    }

    @Override
    public OtpErlangObject checkedGet() throws RpcException {
        try {
            return checkedGet(OtpRpc.INFINITY, TimeUnit.MILLISECONDS);
        } catch (final TimeoutException e) {
            return null;
        }
    }

    @Override
    public OtpErlangObject checkedGet(final long timeout, final TimeUnit unit)
            throws TimeoutException, RpcException {
        result = rpc
                .getRpcResult(mbox, TimeUnit.MILLISECONDS.convert(timeout, unit), env);
        if (isDone()) {
            RpcMonitor.recordResponse(ref, result);
            if (logCalls) {
                final Object[] args0 = { result };
                ErlLogger.debug("call <- %s", args0);
            }
        }
        return result;
    }

}
