package org.erlide.engine.internal.model.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.ErlangFunction;
import org.erlide.engine.model.erlang.IErlImportExport;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public abstract class ErlImportExport extends ErlMember implements IErlImportExport {

    protected List<ErlangFunction> fFunctions;

    protected ErlImportExport(final IParent parent, final String name,
            final OtpErlangList functionList) {
        super(parent, name);
        fFunctions = new ArrayList<ErlangFunction>(functionList.arity());
        for (final OtpErlangObject object : functionList) {
            fFunctions.add(new ErlangFunction((OtpErlangTuple) object));
        }
    }

    @Override
    public boolean hasFunction(final ErlangFunction f) {
        return fFunctions.contains(f);
    }

    @Override
    public Collection<ErlangFunction> getFunctions() {
        return Collections.unmodifiableCollection(Lists.newArrayList(fFunctions));
    }

}
