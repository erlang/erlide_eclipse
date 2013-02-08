package org.erlide.model.internal.erlang;

import org.erlide.model.IParent;
import org.erlide.model.erlang.IErlImport;
import org.erlide.model.erlang.IErlModule;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlImport extends ErlImportExport implements IErlImport {

    String fImportModule;

    /**
     * @param erlModuleInternal
     * @param functionList
     * @param module
     */
    protected ErlImport(final IParent parent, final String importModule,
            final OtpErlangList functionList) {
        super(parent, "import", functionList);
        fImportModule = importModule;
    }

    public ErlImport(final IErlModule parent, final String importModule,
            final OtpErlangList functionList) {
        super(parent, "import", functionList);
        fImportModule = importModule;
    }

    @Override
    public Kind getKind() {
        return Kind.IMPORT;
    }

    @Override
    public String getImportModule() {
        return fImportModule;
    }

    @Override
    public String toString() {
        return getName() + ": " + getImportModule();
    }

    // @Override
    // public OtpErlangObject toErlangObject() {
    // final OtpErlangObject funcs = super.toErlangObject();
    // return OtpErlang.mkTuple(new OtpErlangAtom(getImportModule()), funcs);
    // }
}
