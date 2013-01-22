/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Signature {
    private static final Map<String, Signature[]> CACHE = new HashMap<String, Signature[]>();
    private static boolean useCache = true;

    public char kind = 'x';
    public Signature[] content = null;

    public Signature(final char str) {
        kind = str;
    }

    public Signature(final char crt, final Signature sub) {
        kind = crt;
        content = new Signature[] { sub };
    }

    public Signature(final char crt, final Signature[] sub) {
        kind = crt;
        content = sub;
    }

    @Override
    public String toString() {
        final StringBuilder res = new StringBuilder();
        if (content != null) {
            res.append('(');
            for (final Signature s : content) {
                res.append(s.toString()).append(',');
            }
            res.deleteCharAt(res.length() - 1);
            res.append(')');
        }
        return kind + res.toString();
    }

    public static synchronized Signature[] parse(final String signature)
            throws SignatureException {
        if (signature == null) {
            return null;
        }
        Signature[] result;
        if (useCache) {
            result = CACHE.get(signature);
            if (result != null) {
                return result;
            }
        }
        String sign = signature;
        final List<Signature> type = new ArrayList<Signature>();
        while (sign.length() > 0) {
            final ParseState e = parseOne(sign);
            type.add(e.sign);
            sign = e.rest;
        }
        result = type.toArray(new Signature[type.size()]);
        if (useCache) {
            CACHE.put(signature, result);
        }
        return result;
    }

    private static class ParseState {
        public ParseState(final Signature signature, final String substring) {
            sign = signature;
            rest = substring;
        }

        Signature sign;
        String rest;
    }

    private static ParseState parseOne(final String signature)
            throws SignatureException {
        final char crt = signature.charAt(0);
        if ("xidabrjfpso".indexOf(crt) >= 0) {
            return new ParseState(new Signature(crt), signature.substring(1));
        } else if (crt == 'l') {
            final ParseState sub = parseOne(signature.substring(1));
            return new ParseState(new Signature(crt, sub.sign), sub.rest);
        } else if ("0123456789".indexOf(crt) >= 0) {
            final int numTupleElements = Integer.parseInt(signature.substring(
                    0, 1));
            final Signature[] sub = new Signature[numTupleElements];
            String s = signature.substring(1);
            for (int i = 0; i < numTupleElements; i++) {
                final ParseState state = parseOne(s);
                sub[i] = state.sign;
                s = state.rest;
            }
            return new ParseState(new Signature('t', sub), s);
        } else {
            throw new SignatureException("unknown signature code: " + crt);
        }
    }

    /** To be used only by the unit tests. */
    public static void setUseCache(final boolean use) {
        useCache = use;
    }

}
