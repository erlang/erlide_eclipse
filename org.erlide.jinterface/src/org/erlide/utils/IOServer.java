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
package org.erlide.utils;

import java.util.Collection;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

/**
 * An Erlang io_server.
 */
public class IOServer implements Runnable {

    private final IOCallback callback;
    private final OtpMbox mbox;
    private final Thread thread;
    private volatile boolean stopped = false;

    public IOServer(final OtpMbox box, final IOCallback callback) {
        this.callback = callback;
        mbox = box;
        thread = new Thread(this, "io_server");
        thread.start();
    }

    public void stop() {
        stopped = true;
    }

    @Override
    public void run() {
        boolean done = false;
        do {
            OtpErlangObject msg;
            try {
                msg = mbox.receive(3000);
                if (msg != null) {

                    ErlLogger.debug("IOS " + Thread.currentThread().getName()
                            + " : " + msg);

                    if (msg instanceof OtpErlangTuple) {
                        handleMessage(msg);
                    } else {
                        ErlLogger.debug("IOServer: unknown message " + msg);
                    }
                }
            } catch (final OtpErlangExit e) {
                done = true;
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } while (!stopped || !done || !Thread.interrupted());
        if (stopped) {
            mbox.close();
        }
    }

    private void handleMessage(final OtpErlangObject msg) {
        final OtpErlangTuple tuple = (OtpErlangTuple) msg;
        final String tag = ((OtpErlangAtom) tuple.elementAt(0)).atomValue();
        if ("io_request".equals(tag)) {
            final OtpErlangPid from = (OtpErlangPid) tuple.elementAt(1);
            final OtpErlangObject replyAs = tuple.elementAt(2);
            final OtpErlangTuple request = (OtpErlangTuple) tuple.elementAt(3);
            final OtpErlangObject reply = processRequest(from, request);
            final OtpErlangTuple replyMsg = OtpErlang.mkTuple(
                    new OtpErlangAtom("io_reply"), replyAs, reply);
            mbox.send(from, replyMsg);
        } else {
            ErlLogger.debug("IOServer: unknown message " + msg);
        }
    }

    private final OtpErlangObject error = OtpErlang.mkTuple(new OtpErlangAtom(
            "error"), new OtpErlangAtom("request"));

    private OtpErlangObject processRequest(final OtpErlangPid from,
            final OtpErlangObject arequest) {
        if (callback == null) {
            return error;
        }
        Bindings b;
        try {
            if (arequest instanceof OtpErlangTuple) {
                final OtpErlangTuple request = (OtpErlangTuple) arequest;
                final String tag = ((OtpErlangAtom) request.elementAt(0))
                        .atomValue();
                if ("put_chars".equals(tag)) {
                    b = ErlUtils.match("{put_chars, Chars}", request);
                    if (b != null) {
                        return callback.putChars(from, IOEncoding.latin1,
                                b.get("Chars"));
                    }

                    b = ErlUtils.match("{put_chars, Enc:a, Chars}", request);
                    if (b != null) {
                        final String enc = b.getAtom("Enc");
                        return callback.putChars(from, IOEncoding.valueOf(enc),
                                b.get("Chars"));
                    }

                    b = ErlUtils.match("{put_chars, M:a, F:a, A}", request);
                    if (b != null) {
                        final String m = b.getAtom("M");
                        final String f = b.getAtom("F");
                        final Collection<OtpErlangObject> a = b.getList("A");
                        return callback.putChars(from, IOEncoding.latin1, m, f,
                                a);
                    }

                    b = ErlUtils.match("{put_chars, Enc:a, M:a, F:a, A}",
                            request);
                    if (b != null) {
                        final String enc = b.getAtom("Enc");
                        final String m = b.getAtom("M");
                        final String f = b.getAtom("F");
                        final Collection<OtpErlangObject> a = b.getList("A");
                        return callback.putChars(from, IOEncoding.valueOf(enc),
                                m, f, a);
                    }
                    return error;
                } else if ("get_until".equals(tag)) {
                    b = ErlUtils.match("{get_until, Prompt}", request);
                    if (b != null) {
                        return callback.getUntil(IOEncoding.latin1,
                                b.get("Prompt"));
                    }
                    b = ErlUtils.match("{get_until, Prompt, N:i}", request);
                    if (b != null) {
                        final long n = b.getLong("N");
                        return callback.getUntil(IOEncoding.latin1,
                                b.get("Prompt"), n);
                    }
                    b = ErlUtils.match("{get_until, Enc:a, Prompt}", request);
                    if (b != null) {
                        final String enc = b.getAtom("Enc");
                        return callback.getUntil(IOEncoding.valueOf(enc),
                                b.get("Prompt"));
                    }
                    b = ErlUtils.match("{get_until, Enc:a, Prompt, N:i}",
                            request);
                    if (b != null) {
                        final String enc = b.getAtom("Enc");
                        final long n = b.getLong("N");
                        return callback.getUntil(IOEncoding.valueOf(enc),
                                b.get("Prompt"), n);
                    }
                    b = ErlUtils.match("{get_until, Prompt, M:a, F:a, A}",
                            request);
                    if (b != null) {
                        final String m = b.getAtom("M");
                        final String f = b.getAtom("F");
                        final Collection<OtpErlangObject> a = b.getList("A");
                        return callback.getUntil(IOEncoding.latin1,
                                b.get("Prompt"), m, f, a);
                    }
                    b = ErlUtils
                            .match("{get_until, Enc: a, Prompt, M:a, F:a, A}",
                                    request);
                    if (b != null) {
                        final String enc = b.getAtom("Enc");
                        final String m = b.getAtom("M");
                        final String f = b.getAtom("F");
                        final Collection<OtpErlangObject> a = b.getList("A");
                        return callback.getUntil(IOEncoding.valueOf(enc),
                                b.get("Prompt"), m, f, a);
                    }
                } else if ("requests".equals(tag)) {
                    b = ErlUtils.match("{requests, Reqs:lx}", request);
                    if (b != null) {
                        final Collection<OtpErlangObject> reqs = b
                                .getList("Reqs");
                        OtpErlangObject val = null;
                        for (final OtpErlangObject r : reqs) {
                            val = processRequest(from, r);
                            if (val == error) {
                                return error;
                            }
                        }
                        return val == null ? error : val;
                    }
                } else if ("setopts".equals(tag)) {
                    b = ErlUtils.match("{setopts, Opts:lx}", request);
                    if (b != null) {
                        final Collection<OtpErlangObject> opts = b
                                .getList("Opts");
                        return callback.setOpts(opts);
                    }
                } else if ("get_geometry".equals(tag)) {
                    return OtpErlang.mkTuple(new OtpErlangAtom("error"),
                            new OtpErlangAtom("enotsup"));
                } else {
                    return error;
                }
            } else if (arequest instanceof OtpErlangAtom) {
                final OtpErlangAtom tag = (OtpErlangAtom) arequest;
                if ("getopts".equals(tag.atomValue())) {
                    return callback.getOpts();
                } else {
                    return error;
                }
            } else {
                return error;
            }
        } catch (final TermParserException e) {
            e.printStackTrace();
        } catch (final OtpErlangException e) {
            e.printStackTrace();
        }
        return error;
    }
}
