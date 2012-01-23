/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.TypeConverter;
import org.erlide.jinterface.internal.BindingsImpl;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpFormatPlaceholder;
import com.ericsson.otp.erlang.OtpPatternVariable;
import com.ericsson.otp.erlang.Signature;
import com.ericsson.otp.erlang.SignatureException;

public final class ErlUtils {

    private ErlUtils() {
    }

    final static private TermParser termParser = TermParser.getParser();

    public static OtpErlangObject parse(final String string)
            throws TermParserException {
        return termParser.parse(string);
    }

    /**
     * Build an Erlang (extended) term from a textual description. For example,
     * <code> format("{hello, ~s, [~a, _]}", "myname", "mykey")
     * </code> gives the equivalent of <code> {hello, "myname", [mykey, _]}
     * </code>.
     * <p>
     * Items beginning with ~ are placeholders that will be replaced with the
     * corresponding argument (from left to right). The text after the ~ is the
     * type signature of the argument, so that automatic conversion Java->Erlang
     * can be done. See TypeConverter.java2erlang for details.
     * 
     * @see org.erlide.jinterface.TypeConverter
     */
    public static OtpErlangObject format(final String fmt, final Object... args)
            throws TermParserException, SignatureException {
        final List<Object> values = new ArrayList<Object>(Arrays.asList(args));
        final OtpErlangObject result = fill(parse(fmt), values);
        return result;
    }

    public static Bindings match(final String pattern, final String term)
            throws TermParserException {
        return match(parse(pattern), parse(term), new BindingsImpl());
    }

    public static Bindings match(final String pattern,
            final OtpErlangObject term) throws TermParserException {
        return match(parse(pattern), term, new BindingsImpl());
    }

    public static Bindings match(final String pattern,
            final OtpErlangObject term, final Bindings bindings)
            throws TermParserException {
        return match(parse(pattern), term, bindings);
    }

    public static Bindings match(final String pattern, final String term,
            final Bindings bindings) throws TermParserException {
        return match(parse(pattern), parse(term), bindings);
    }

    public static Bindings match(final OtpErlangObject pattern,
            final OtpErlangObject term) {
        return match(pattern, term, new BindingsImpl());
    }

    public static Bindings match(final OtpErlangObject pattern,
            final String term) throws TermParserException {
        return match(pattern, parse(term), new BindingsImpl());
    }

    /**
     * Match two Erlang terms.
     * <p>
     * Patterns have an extended syntax:
     * <ul>
     * <li>Variables can have a type signature attached, like for example
     * <code>Var:i</code>. Its meaning is that the type of the value must match
     * too.</li>
     * <li>The tail of a list can only be a variable.</li>
     * </ul>
     * <p>
     * The returned value is null if there was any mismatch, otherwise it is a
     * map of variable names to matched values. <br>
     * TODO should we throw an exception instead?
     * 
     */
    public static Bindings match(final OtpErlangObject pattern,
            final OtpErlangObject term, final Bindings bindings) {
        if (pattern == null && term == null) {
            return bindings;
        }
        if (pattern == null || term == null) {
            return null;
        }
        if (pattern instanceof OtpPatternVariable) {
            final OtpPatternVariable var = (OtpPatternVariable) pattern;
            if (!TypeConverter.doesMatchSignature(term, var.getSignature())) {
                return null;
            }
            if (var.getName().equals("_")) {
                return bindings;
            }
            final Bindings result = new BindingsImpl(bindings);
            final OtpErlangObject old = bindings.get(var.getName());
            if (old == null) {
                // no previous binding
                result.put(var.getName(), term);
                return result;
            } else {
                return old.equals(term) ? result : null;
            }
        }
        if (!pattern.getClass().equals(term.getClass())) {
            return null;
        }

        if (pattern.equals(term)) {
            return bindings;
        } else if (pattern instanceof OtpErlangList) {
            return matchList(pattern, term, bindings);
        } else if (pattern instanceof OtpErlangTuple) {
            return matchTuple(((OtpErlangTuple) pattern).elements(),
                    ((OtpErlangTuple) term).elements(), bindings, false);
        }
        return null;
    }

    private static Bindings matchList(final OtpErlangObject pattern,
            final OtpErlangObject term, final Bindings bindings) {
        final OtpErlangList lpattern = (OtpErlangList) pattern;
        final OtpErlangList lterm = (OtpErlangList) term;
        final int patternArity = lpattern.arity();
        final int termArity = lterm.arity();
        if (patternArity > termArity) {
            return null;
        }
        if (patternArity < termArity && lpattern.isProper()) {
            return null;
        }
        if (patternArity == termArity
                && lpattern.isProper() != lterm.isProper()) {
            return null;
        }
        Bindings rez = bindings;
        for (int i = 0; i < patternArity; i++) {
            rez = match(lpattern.elementAt(i), lterm.elementAt(i), rez);
            if (rez == null) {
                return null;
            }
        }
        if (patternArity == termArity) {
            rez = match(lpattern.getLastTail(), lterm.getLastTail(), rez);
            return rez;
        }
        if (lpattern.getLastTail() instanceof OtpPatternVariable) {
            return match(lpattern.getLastTail(),
                    lterm.getNthTail(patternArity), rez);
        }
        return match(lpattern.getLastTail(), lterm.getLastTail(), rez);
    }

    private static OtpErlangObject fill(final OtpErlangObject template,
            final List<Object> values) throws SignatureException,
            TermParserException {
        if (values.size() == 0) {
            return template;
        }
        if (template instanceof OtpErlangList) {
            final OtpErlangObject[] elements = ((OtpErlangList) template)
                    .elements();
            final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
                    elements.length);
            for (final OtpErlangObject elem : elements) {
                result.add(fill(elem, values));
            }
            return new OtpErlangList(result.toArray(elements));
        } else if (template instanceof OtpErlangTuple) {
            final OtpErlangObject[] elements = ((OtpErlangTuple) template)
                    .elements();
            final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
                    elements.length);
            for (final OtpErlangObject elem : elements) {
                result.add(fill(elem, values));
            }
            return new OtpErlangTuple(result.toArray(elements));
        } else if (template instanceof OtpFormatPlaceholder) {
            final OtpFormatPlaceholder holder = (OtpFormatPlaceholder) template;
            final Object ret = values.remove(0);
            final Signature[] signs = Signature.parse(holder.getName());
            if (signs.length == 0 && !(ret instanceof OtpErlangObject)) {
                throw new TermParserException("funny placeholder");
            }
            final Signature sign = signs.length == 0 ? new Signature('x')
                    : signs[0];
            return TypeConverter.java2erlang(ret, sign);
        } else {
            return template;
        }
    }

    private static Bindings matchTuple(final OtpErlangObject[] patterns,
            final OtpErlangObject[] terms, final Bindings bindings,
            final boolean list) {
        if (patterns.length != terms.length) {
            return null;
        }
        Bindings result = new BindingsImpl(bindings);
        for (int i = 0; i < patterns.length; i++) {
            result = match(patterns[i], terms[i], result);
            if (result == null) {
                return null;
            }
        }
        return result;
    }

    /**
     * This is useful if a value can be anything, but we need to see it as a
     * string (but without any quotes if it really is a string).
     * 
     * @param target
     * @return
     */
    public static String asString(final OtpErlangObject target) {
        if (target instanceof OtpErlangAtom) {
            return ((OtpErlangAtom) target).atomValue();
        } else if (target instanceof OtpErlangString) {
            return ((OtpErlangString) target).stringValue();
        } else if (target instanceof OtpErlangBinary) {
            return new String(((OtpErlangBinary) target).binaryValue());
        } else if (target instanceof OtpErlangList) {
            try {
                return ((OtpErlangList) target).stringValue();
            } catch (final OtpErlangException e) {
                return target.toString();
            }
        } else {
            return target.toString();
        }
    }
}
