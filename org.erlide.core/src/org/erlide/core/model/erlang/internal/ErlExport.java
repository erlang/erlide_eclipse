package org.erlide.core.model.erlang.internal;

import org.erlide.core.model.erlang.IErlExport;
import org.erlide.core.model.erlang.IParent;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

    private final String functions;

    protected ErlExport(final IParent parent, final OtpErlangList functionList,
            final String functions) {
        super(parent, "export", functionList);
        this.functions = functions;
    }

    public Kind getKind() {
        return Kind.EXPORT;
    }

    @Override
    public String toString() {
        return getName() + ": " + functions;
    }

}
