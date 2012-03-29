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

package org.erlide.core.model.erlang;

import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class ErlToken {

    private static final boolean TRACE = false;

    public static final int KIND_OTHER = 0;
    public static final int KIND_WHITESPACE = 1;
    public static final int KIND_STRING = 2;
    public static final int KIND_ATOM = 3;
    public static final int KIND_VAR = 4;
    public static final int KIND_CHAR = 5;
    public static final int KIND_MACRO = 6;
    public static final int KIND_ARROW = 7;
    public static final int KIND_INTEGER = 8;
    public static final int KIND_FLOAT = 9;
    public static final int KIND_COMMENT = 10;
    public static final int KIND_KEYWORD = 11;

    private int kind;
    private int length;
    private int offset;
    private int line;
    // private String text;

    public static final ErlToken EOF = new ErlToken(KIND_OTHER);

    // special for lightscanstring
    // <<(kind_small(Kind)), L:24, O:24, G:24>>
    public ErlToken(final byte[] bytes, final int index) {
        kind = bytes[index];
        line = int24(bytes, index + 1);
        offset = int24(bytes, index + 4);
        length = int24(bytes, index + 7);
    }

    private int int24(final byte[] bytes, final int index) {
        return (0xff & bytes[index]) << 16 | (0xff & bytes[index + 1]) << 8
                | 0xff & bytes[index + 2];
    }

    public ErlToken(final OtpErlangTuple e) {
        // fTuple = e;
        if (TRACE) {
            ErlLogger.debug("    =" + e.toString());
        }

        final OtpErlangObject[] parts = e.elements();
        if (((OtpErlangAtom) parts[0]).atomValue().equals("token")
                && parts.length == 8) {

            // -record(token, {kind, line, offset, length, value, text}).
            kind = KIND_OTHER;
            if (parts[1] instanceof OtpErlangAtom) {
                final String s = ((OtpErlangAtom) parts[1]).atomValue();
                if (s.equals("eof")) {
                    return;
                } else if (s.equals("ws")) {
                    kind = KIND_WHITESPACE;
                } else if (s.equals("string")) {
                    kind = KIND_STRING;
                } else if (s.equals("atom")) {
                    kind = KIND_ATOM;
                } else if (s.equals("var")) {
                    kind = KIND_VAR;
                } else if (s.equals("char")) {
                    kind = KIND_CHAR;
                } else if (s.equals("macro")) {
                    kind = KIND_MACRO;
                } else if (s.equals("->")) {
                    kind = KIND_ARROW;
                } else if (s.equals("integer")) {
                    kind = KIND_INTEGER;
                } else if (s.equals("float")) {
                    kind = KIND_FLOAT;
                } else if (s.equals("comment")) {
                    kind = KIND_COMMENT;
                } else {
                    kind = KIND_OTHER;
                }
            } else {
                try {
                    final OtpErlangLong l = (OtpErlangLong) parts[1];
                    kind = l.intValue();
                } catch (final OtpErlangRangeException e1) {
                    kind = KIND_OTHER;
                }
            }

            try {
                line = ((OtpErlangLong) parts[2]).intValue();
                offset = ((OtpErlangLong) parts[3]).intValue();
                length = ((OtpErlangLong) parts[4]).intValue();
            } catch (final OtpErlangRangeException e1) {
                ErlLogger.warn(e1);
            } // value = ((OtpErlangAtom) parts[5]).atomValue();
              // if (parts[6] instanceof OtpErlangString) {
              // text = ((OtpErlangString) parts[6]).stringValue();
              // } else {
              // text = parts[6].toString();
              // if ("undefined".equals(text) || "u".equals(text)) {
              // text = parts[5].toString();
              // }
              // }
              //
            if (TRACE) {
                ErlLogger.debug("mkTok " + kind + " - " + line + "/" + offset
                        + ":" + length);
                // ErlLogger.debug("mkTok " + kind + " - " + text + " " + line +
                // "/"
                // + offset + ": '" + text + "' ");
            }
        } else {
            // {kind, line, offset, length} | {kind, line, offset, length, _} |
            // {kind, line, offset, length, _, _}
            try {
                kind = ((OtpErlangLong) parts[0]).intValue();
                line = ((OtpErlangLong) parts[1]).intValue();
                offset = ((OtpErlangLong) parts[2]).intValue();
                length = ((OtpErlangLong) parts[3]).intValue();
            } catch (final OtpErlangRangeException e1) {
                ErlLogger.warn(e1);
            } // value = ((OtpErlangAtom) parts[5]).atomValue();
        }
    }

    // eof token
    private ErlToken(final int kind) {
        this.kind = kind;
    }

    public int getKind() {
        return kind;
    }

    public int getLength() {
        return length;
    }

    public int getOffset() {
        return offset;
    }

    @Override
    public String toString() {
        return "{" + kind + ", " + line + "/" + offset + "+" + length + "}";
        // return "{" + kind + ", " + line + "/" + offset + "+" + length + ": "
        // + text + "}";
    }

    /**
     * @param ofs
     */
    public void fixOffset(final int ofs) {
        offset += ofs;
    }
}
