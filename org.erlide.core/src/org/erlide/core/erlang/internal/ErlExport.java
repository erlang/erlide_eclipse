package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IErlModule;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

    private final String functions;

    protected ErlExport(final IErlModule module,
            final OtpErlangList functionList, final String functions) {
        super(module, "export", functionList);
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
