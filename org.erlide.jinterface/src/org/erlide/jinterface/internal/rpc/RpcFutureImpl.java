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
package org.erlide.jinterface.internal.rpc;

import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcHelper;
import org.erlide.jinterface.rpc.RpcMonitor;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpMbox;

public class RpcFutureImpl implements IRpcFuture {

    private final OtpMbox mbox;
    private OtpErlangObject result = null;
    private final String env;
    private final boolean logCalls;

    private final RpcHelper helper;
    private final OtpErlangRef ref;

    public RpcFutureImpl(final OtpErlangRef ref, final OtpMbox mbox,
            final String env, final boolean logCalls, final RpcHelper helper) {
        this.ref = ref;

        this.mbox = mbox;
        this.env = env;
        this.logCalls = logCalls;
        this.helper = helper;
    }

    @Override
    public OtpErlangObject get() throws RpcException {
        return get(RpcHelper.INFINITY);
    }

    @Override
    public OtpErlangObject get(final long timeout) throws RpcException {
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
    public boolean isDone() {
        return result != null;
    }

}
