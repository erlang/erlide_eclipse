package com.ericsson.otp.erlang;

import java.util.Collection;
import java.util.List;

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

    public static OtpErlangList mkStringList(final Collection<String> args) {
        final List<OtpErlangObject> result = Lists.newArrayList();
        for (final String s : args) {
            result.add(new OtpErlangString(s));
        }
        return mkList(result);
    }

}
