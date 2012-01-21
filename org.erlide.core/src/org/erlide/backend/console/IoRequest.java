/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.console;

import java.util.regex.Pattern;

import org.eclipse.core.runtime.Assert;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IoRequest {

    public static final Pattern RE_PROMPT = Pattern
            .compile("\\([^)]+\\)[0-9]+> |[0-9]+> ");
    public static final Pattern RE_HEADER = Pattern
            .compile("Eshell V[0-9]+\\.[0-9]+\\.[0-9]+");

    public enum IoRequestKind {
        HEADER, PROMPT, INPUT, OUTPUT, STDOUT, STDERR;
    }

    private OtpErlangPid leader;
    private OtpErlangPid sender;
    private String message;
    private int start;
    private final IoRequestKind kind;

    public IoRequest(final OtpErlangTuple obj) {
        try {
            final OtpErlangObject o = obj.elementAt(0);
            if (o instanceof OtpErlangString) {
                message = ((OtpErlangString) o).stringValue();
            } else if (o instanceof OtpErlangList) {
                final OtpErlangList l = (OtpErlangList) o;
                if (l.arity() == 0) {
                    message = "";
                } else {
                    try {
                        message = l.stringValue();
                    } catch (final Exception e) {
                        message = o.toString();
                    }
                }
            } else {
                message = o.toString();
            }

            leader = (OtpErlangPid) obj.elementAt(1);
            final OtpErlangObject s = obj.elementAt(2);
            if (s instanceof OtpErlangPid) {
                sender = (OtpErlangPid) s;
            } else {
                sender = new OtpErlangPid("s", 0, 0, 0);
            }
        } catch (final Exception e) {
            message = null;
        }
        if (RE_PROMPT.matcher(message).matches()) {
            kind = IoRequestKind.PROMPT;
        } else if (RE_HEADER.matcher(message).matches()) {
            kind = IoRequestKind.HEADER;
        } else {
            kind = IoRequestKind.OUTPUT;
        }
    }

    public IoRequest(final String msg, final IoRequestKind kind) {
        Assert.isTrue(kind != IoRequestKind.OUTPUT);
        Assert.isTrue(kind != IoRequestKind.PROMPT);
        message = msg;
        leader = new OtpErlangPid("s", 0, 0, 0);
        sender = new OtpErlangPid("s", 0, 0, 0);
        this.kind = kind;
    }

    @Override
    public String toString() {
        return "{" + kind.toString() + ":: '" + message + "', " + start + "/"
                + message.length() + ", " + leader + ", " + sender + "}";
    }

    public OtpErlangPid getLeader() {
        return leader;
    }

    public String getMessage() {
        return message;
    }

    public OtpErlangPid getSender() {
        return sender;
    }

    public int getStart() {
        return start;
    }

    public int getLength() {
        return message.length();
    }

    public IoRequestKind getKind() {
        return kind;
    }

    public void setStart(final int pos) {
        start = pos;
    }
}
