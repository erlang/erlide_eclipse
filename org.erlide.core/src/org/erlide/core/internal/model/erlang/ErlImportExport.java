package org.erlide.core.internal.model.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.erlide.core.internal.model.root.ErlMember;
import org.erlide.core.model.erlang.IErlImportExport;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Lists;

public abstract class ErlImportExport extends ErlMember implements IParent,
        IErlImportExport {

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
        return Collections.unmodifiableCollection(Lists
                .newArrayList(fFunctions));
    }

}
