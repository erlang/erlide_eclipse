package org.erlide.model.internal.erlang;

import org.erlide.model.IParent;
import org.erlide.model.erlang.IErlExport;
import org.erlide.model.root.ErlElementKind;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

    private final String functions;

    public ErlExport(final IParent parent, final OtpErlangList functionList,
            final String functions) {
        super(parent, "export", functionList);
        this.functions = functions;
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.EXPORT;
    }

    @Override
    public String toString() {
        return getName() + ": " + functions;
    }

}
