package org.erlide.core.erlang.internal;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IParent;

public class ErlExternalReferenceEntry extends Openable implements IErlExternal {

    private final String entry;
    private final boolean isRoot;

    protected ErlExternalReferenceEntry(final IParent parent,
            final String name, final String entry, final boolean isRoot) {
        super(parent, name);
        this.entry = entry;
        this.isRoot = isRoot;
    }

    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        // already done
        return true;
    }

    @Override
    protected void closing(final Object info) throws ErlModelException {
        // TODO Auto-generated method stub
    }

    @Override
    public String getFilePath() {
        return null;
    }

    @Override
    public String getLabelString() {
        return super.getName();
    }

    public String getExternalName() {
        return entry;
    }

    // public boolean hasModuleWithPath(final String path) {
    // final Backend backend = BackendUtils
    // .getBuildOrIdeBackend(getErlProject().getProject());
    // final OtpErlangList pathVars = ErlangCore.getModel().getPathVars();
    // return ErlideOpen.hasExternalWithPath(backend, entry, path, pathVars);
    // }

    public boolean isRoot() {
        return isRoot;
    }

}
