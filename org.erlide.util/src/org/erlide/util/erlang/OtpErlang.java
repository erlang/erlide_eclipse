package org.erlide.util.erlang;

import java.io.IOException;
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

    public static TermParser getTermParser() {
        return new TermParser();
    }
}
