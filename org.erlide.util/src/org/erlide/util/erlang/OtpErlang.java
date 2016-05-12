package org.erlide.util.erlang;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpOutputStream;
import com.google.common.collect.Lists;

public class OtpErlang {

    public static OtpErlangList mkList(final OtpErlangObject... args) {
        return new OtpErlangList(args);
    }

    public static OtpErlangList mkList(final Collection<OtpErlangObject> args) {
        return new OtpErlangList(args.toArray(new OtpErlangObject[args.size()]));
    }

    public static OtpErlangTuple mkTuple(final OtpErlangObject... args) {
        return new OtpErlangTuple(args);
    }

    public static OtpErlangList mkStringList(final Collection<?> args) {
        final List<OtpErlangObject> result = Lists.newArrayList();
        for (final Object s : args) {
            result.add(new OtpErlangString(s.toString()));
        }
        return mkList(result);
    }

    // for debugging purposes
    public static long sizeOf(final OtpErlangObject obj) {
        final OtpOutputStream buf = new OtpOutputStream(obj);
        try {
            return buf.size();
        } finally {
            try {
                buf.close();
            } catch (final IOException e) {
            }
        }
    }

    public static OtpParser getTermParser() {
        return new OtpParser();
    }

    final private static OtpParser TERM_PARSER = OtpErlang.getTermParser();

    public static OtpErlangObject parse(final String string) throws OtpParserException {
        return TERM_PARSER.parse(string);
    }

    /**
     * Build an Erlang (extended) term from a textual description. For example,
     * <code> format("{hello, ~s, [~a, _]}", "myname", "mykey")
     * </code> gives the equivalent of <code> {hello, "myname", [mykey, _]}
     * </code>.
     * <p>
     * Items beginning with ~ are placeholders that will be replaced with the
     * corresponding argument (from left to right). The text after the ~ is the type
     * signature of the argument, so that automatic conversion Java->Erlang can be done.
     * See TypeConverter.java2erlang for details.
     *
     * @see com.ericsson.otp.erlang.TypeConverter
     */
    public static OtpErlangObject format(final String fmt, final Object... args)
            throws OtpParserException, SignatureException {
        final List<Object> values = new ArrayList<Object>(Arrays.asList(args));
        final OtpErlangObject result = fill(parse(fmt), values);
        return result;
    }

    public static OtpBindings match(final String pattern, final String term)
            throws OtpParserException {
        return match(parse(pattern), parse(term), new OtpBindings());
    }

    public static OtpBindings match(final String pattern, final OtpErlangObject term)
            throws OtpParserException {
        return match(parse(pattern), term, new OtpBindings());
    }

    public static OtpBindings match(final String pattern, final String term,
            final OtpBindings bindings) throws OtpParserException {
        return match(parse(pattern), parse(term), bindings);
    }

    public static OtpBindings match(final OtpErlangObject pattern,
            final OtpErlangObject term) {
        return match(pattern, term, new OtpBindings());
    }

    /**
     * Match two Erlang terms.
     * <p>
     * Patterns have an extended syntax:
     * <ul>
     * <li>Variables can have a type signature attached, like for example
     * <code>Var:i</code>. Its meaning is that the type of the value must match too.</li>
     * <li>The tail of a list can only be a variable.</li>
     * </ul>
     * <p>
     * The returned value is null if there was any mismatch, otherwise it is a map of
     * variable names to matched values. <br>
     *
     */
    public static OtpBindings match(final OtpErlangObject pattern,
            final OtpErlangObject term, final OtpBindings bindings) {
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
            final OtpBindings result = new OtpBindings(bindings);
            final OtpErlangObject old = bindings.get(var.getName());
            if (old == null) {
                // no previous binding
                result.put(var.getName(), term);
                return result;
            }
            return old.equals(term) ? result : null;
        }
        if (!pattern.getClass().equals(term.getClass())) {
            return null;
        }

        if (pattern instanceof OtpErlangList) {
            return matchList(pattern, term, bindings);
        } else if (pattern instanceof OtpErlangTuple) {
            return matchTuple(((OtpErlangTuple) pattern).elements(),
                    ((OtpErlangTuple) term).elements(), bindings);
        } else if (pattern.equals(term)) {
            return bindings;
        }
        return null;
    }

    private static OtpBindings matchList(final OtpErlangObject pattern,
            final OtpErlangObject term, final OtpBindings bindings) {
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
        if (patternArity == termArity && lpattern.isProper() != lterm.isProper()) {
            return null;
        }
        OtpBindings rez = bindings;
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
            return match(lpattern.getLastTail(), lterm.getNthTail(patternArity), rez);
        }
        return match(lpattern.getLastTail(), lterm.getLastTail(), rez);
    }

    private static OtpErlangObject fill(final OtpErlangObject template,
            final List<Object> values) throws SignatureException, OtpParserException {
        if (values.isEmpty()) {
            return template;
        }
        if (template instanceof OtpErlangList) {
            final OtpErlangObject[] elements = ((OtpErlangList) template).elements();
            final List<OtpErlangObject> result = new ArrayList<OtpErlangObject>(
                    elements.length);
            for (final OtpErlangObject elem : elements) {
                result.add(fill(elem, values));
            }
            return new OtpErlangList(result.toArray(elements));
        } else if (template instanceof OtpErlangTuple) {
            final OtpErlangObject[] elements = ((OtpErlangTuple) template).elements();
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
                throw new OtpParserException("funny placeholder");
            }
            final Signature sign = signs.length == 0 ? new Signature('x') : signs[0];
            return TypeConverter.java2erlang(ret, sign);
        } else {
            return template;
        }
    }

    private static OtpBindings matchTuple(final OtpErlangObject[] patterns,
            final OtpErlangObject[] terms, final OtpBindings bindings) {
        if (patterns.length != terms.length) {
            return null;
        }
        OtpBindings result = new OtpBindings(bindings);
        for (int i = 0; i < patterns.length; i++) {
            result = match(patterns[i], terms[i], result);
            if (result == null) {
                return null;
            }
        }
        return result;
    }

    /**
     * This is useful if a value can be anything, but we need to see it as a string (but
     * without any quotes if it really is a string).
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
