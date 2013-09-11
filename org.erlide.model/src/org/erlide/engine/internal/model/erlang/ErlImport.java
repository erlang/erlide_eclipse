package org.erlide.engine.internal.model.erlang;

import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlImport;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlElementKind;

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
    public ErlElementKind getKind() {
        return ErlElementKind.IMPORT;
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
