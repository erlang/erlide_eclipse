package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public abstract class ErlImportExport extends ErlMember implements IParent {

    protected List<ErlangFunction> fFunctions;

    protected ErlImportExport(final IParent parent, final String name,
            final OtpErlangList functionList) {
        super(parent, name);
        fFunctions = new ArrayList<ErlangFunction>(functionList.arity());
        for (final OtpErlangObject object : functionList) {
            fFunctions.add(new ErlangFunction((OtpErlangTuple) object));
        }
    }

    public boolean hasFunction(final ErlangFunction f) {
        return fFunctions.contains(f);
    }

    public Collection<ErlangFunction> getFunctions() {
        return Collections.unmodifiableCollection(Lists
                .newArrayList(fFunctions));
    }

    public OtpErlangObject toErlangObject() {
        final OtpErlangObject[] funcTuples = new OtpErlangObject[fFunctions
                .size()];
        int i = 0;
        for (final ErlangFunction f : fFunctions) {
            funcTuples[i++] = OtpErlang.mkTuple(new OtpErlangAtom(f.name),
                    new OtpErlangLong(f.arity));
        }
        return new OtpErlangList(funcTuples);
    }

}
