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
package org.erlide.jinterface.rpc;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * This is a thread driven by a mailbox, that waits for rpc results and
 * dispatches them to registered callback.
 * 
 * Protocol:
 * <ul>
 * <li>caller starts receiver and registers callback</li>
 * <li>caller sends request via rpc:cast()</li>
 * <li>receiver waits for "start" message; calls callback</li>
 * <li>receiver waits for "progress" messages; calls callback for each one</li>
 * <li>receiver waits for "stop" message, calls callback and quits</li>
 * </ul>
 */
public class RpcResultReceiver implements Runnable {

    private final IRpcResultCallback callback;
    private final OtpMbox mbox;

    public RpcResultReceiver(final OtpMbox box,
            final IRpcResultCallback callback) {
        this.callback = callback;
        mbox = box;
        new Thread(this, "rpc").start();
    }

    @Override
    public void run() {
        boolean done = false;
        do {
            OtpErlangObject msg;
            try {
                msg = mbox.receive(3000);
                if (msg != null) {
                    if (msg instanceof OtpErlangTuple) {
                        final OtpErlangTuple tuple = (OtpErlangTuple) msg;
                        final String tag = ((OtpErlangAtom) tuple.elementAt(0))
                                .atomValue();
                        if ("start".equals(tag)) {
                            callback.start(tuple.elementAt(1));
                        } else if ("stop".equals(tag)) {
                            done = true;
                            callback.stop(tuple.elementAt(1));
                        } else if ("progress".equals(tag)) {
                            callback.progress(tuple.elementAt(1));
                        }
                    }
                }
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } while (!done || Thread.interrupted());
    }
}
