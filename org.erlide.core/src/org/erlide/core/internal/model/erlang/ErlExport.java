package org.erlide.core.internal.model.erlang;

import org.erlide.core.model.erlang.IErlExport;
import org.erlide.core.model.root.IParent;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

    private final String functions;

    public ErlExport(final IParent parent, final OtpErlangList functionList,
            final String functions) {
        super(parent, "export", functionList);
        this.functions = functions;
    }

    @Override
    public Kind getKind() {
        return Kind.EXPORT;
    }

    @Override
    public String toString() {
        return getName() + ": " + functions;
    }

}
