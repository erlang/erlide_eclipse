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
package org.erlide.runtime.internal.rpc;

import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.IRpcHelper;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcMonitor;
import org.erlide.runtime.rpc.RpcTimeoutException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpMbox;

public class RpcFutureImpl implements IRpcFuture {

    private final OtpMbox mbox;
    private OtpErlangObject result = null;
    private final String env;
    private final boolean logCalls;

    private final IRpcHelper helper;
    private final OtpErlangRef ref;

    public RpcFutureImpl(final OtpErlangRef ref, final OtpMbox mbox,
            final String env, final boolean logCalls, final IRpcHelper helper) {
        this.ref = ref;

        this.mbox = mbox;
        this.env = env;
        this.logCalls = logCalls;
        this.helper = helper;
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
        // TODO Auto-generated method stub
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
            return checkedGet(IRpcHelper.INFINITY, TimeUnit.MILLISECONDS);
        } catch (final TimeoutException e) {
            return null;
        }
    }

    @Override
    public OtpErlangObject checkedGet(final long timeout, final TimeUnit unit)
            throws TimeoutException, RpcException {
        if (isDone()) {
            if (logCalls) {
                helper.debugLogCallArgs("call <- %s", result);
            }
            return result;
        }
        result = helper.getRpcResult(mbox, timeout, env);
        if (isDone()) {
            RpcMonitor.recordResponse(ref, result);
            if (logCalls) {
                helper.debugLogCallArgs("call <- %s", result);
            }
        }
        return result;
    }

    @Override
    public OtpErlangObject get(final long timeout) {
        return get(timeout, TimeUnit.MILLISECONDS);
    }

    @Override
    public OtpErlangObject checkedGet(final long timeout) throws RpcException {
        try {
            return checkedGet(timeout, TimeUnit.MILLISECONDS);
        } catch (final TimeoutException e) {
            throw new RpcTimeoutException(e.getMessage());
        }
    }

}
