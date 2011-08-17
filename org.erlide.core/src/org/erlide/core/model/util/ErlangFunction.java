package org.erlide.core.model.util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangFunction {

    public static final int ANY_ARITY = -1;
    public String name;
    public int arity;

    /**
     * @param name
     * @param arity
     */
    public ErlangFunction(final String name, final int arity) {
        super();
        this.name = name;
        this.arity = arity;
    }

    public ErlangFunction(final String name) {
        super();
        this.name = name;
        arity = ANY_ARITY;
    }

    public ErlangFunction(final OtpErlangTuple tuple) {
        OtpErlangAtom a;
        arity = 0;
        if (tuple.arity() == 2) {
            a = (OtpErlangAtom) tuple.elementAt(0);
            final OtpErlangLong l = (OtpErlangLong) tuple.elementAt(1);
            try {
                arity = l.intValue();
            } catch (final OtpErlangRangeException e) {
            }
        } else {
            a = (OtpErlangAtom) tuple.elementAt(2);
            final OtpErlangObject parameters = tuple.elementAt(3);
            if (parameters instanceof OtpErlangLong) {
                final OtpErlangLong l = (OtpErlangLong) parameters;
                try {
                    arity = l.intValue();
                } catch (final OtpErlangRangeException e) {
                }
            } else if (parameters instanceof OtpErlangList) {
                final OtpErlangList l = (OtpErlangList) parameters;
                arity = l.arity();
            }
        }
        name = a.atomValue();
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj instanceof ErlangFunction) {
            final ErlangFunction f = (ErlangFunction) obj;
            if (f.name.equals(name)) {
                return f.arity == arity || f.arity == ErlangFunction.ANY_ARITY
                        || arity == ErlangFunction.ANY_ARITY;
            }
        }
        return false;
    }

    @Override
    public int hashCode() {
        return arity * 123 + name.hashCode();
    }

    @Override
    public String toString() {
        return name + "/" + Integer.toString(arity);
    }

    public String getNameWithArity() {
        return toString();
    }

    public String getNameWithParameters() {
        return ErlangFunction.getNameWithParameters(name, arity);
    }

    public OtpErlangTuple getNameArityTuple() {
        return new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom(name), new OtpErlangLong(arity) });
    }

    public static String getNameWithParameters(final String name,
            final int arity) {
        final StringBuilder b = new StringBuilder();
        b.append(name).append('(');
        for (int i = 0; i < arity; i++) {
            b.append('_');
            if (i < arity - 1) {
                b.append(", ");
            }
        }
        b.append(')');
        return b.toString();
    }
}
