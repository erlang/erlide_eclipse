package org.erlide.util.erlang;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

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
            final OtpErlangObject term, final OtpBindings bindings) {
        final OtpBindings newBindings = ErlUtils.match(pattern, term, bindings);
        if (newBindings != null) {
            return bindings;
        }
        return null;
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

}
