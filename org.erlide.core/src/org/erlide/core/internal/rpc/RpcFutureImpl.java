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
package org.erlide.core.internal.rpc;

import org.erlide.core.rpc.IRpcFuture;
import org.erlide.core.rpc.IRpcHelper;
import org.erlide.core.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;

public class RpcFutureImpl implements IRpcFuture {

    private final OtpMbox mbox;
    private OtpErlangObject result = null;
    private final String env;
    private final boolean logCalls;
    private final IRpcHelper helper;

    public RpcFutureImpl(final OtpMbox mbox, final String env,
            final boolean logCalls, final IRpcHelper helper) {
        this.mbox = mbox;
        this.env = env;
        this.logCalls = logCalls;
        this.helper = helper;
    }

    public OtpErlangObject get() throws RpcException {
        if (isDone()) {
            if (logCalls) {
                helper.debugLogCallArgs("call <- %s", result);
            }
            return result;
        }
        return get(IRpcHelper.INFINITY);
    }

    public OtpErlangObject get(final long timeout) throws RpcException {
        if (isDone()) {
            return result;
        }
        result = helper.getRpcResult(mbox, timeout, env);
        if (logCalls) {
            helper.debugLogCallArgs("call <- %s", result);
        }
        return result;
    }

    public boolean isDone() {
        return result != null;
    }

}
