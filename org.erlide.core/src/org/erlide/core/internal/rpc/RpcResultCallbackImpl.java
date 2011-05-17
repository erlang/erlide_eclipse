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
package org.erlide.core.internal.rpc;

import org.erlide.core.rpc.IRpcResultCallback;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Groups callbacks for {@link RpcHelper#rpcCallWithProgress}. Clients should
 * extend it locally.
 * 
 */
public class RpcResultCallbackImpl implements IRpcResultCallback {
    public void start(final OtpErlangObject msg) {
    }

    public void stop(final OtpErlangObject msg) {
    }

    public void progress(final OtpErlangObject msg) {
    }
}
