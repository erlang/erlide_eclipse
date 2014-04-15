package org.erlide.engine.internal.model.erlang;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.Openable;
import org.erlide.engine.internal.util.BackendUtil;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenService;
import org.erlide.runtime.api.IRpcSite;

public class ErlOtpExternalReferenceEntryList extends Openable implements
        IErlExternalRoot {

    public ErlOtpExternalReferenceEntryList(final IParent parent,
            final String name) {
        super(parent, name);
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.EXTERNAL;
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final IErlProject erlProject = ErlangEngine.getInstance()
                .getModelUtilService().getProject(this);
        final IRpcSite backend = new BackendUtil().getBackend(erlProject
                .getWorkspaceProject());
        if (backend != null) {
            addExternalEntries(pm, backend);
        }
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm,
            final IRpcSite backend) {
        final IErlExternalRoot structure = ErlangEngine.getInstance()
                .getService(OpenService.class).getOtpLibStructure(backend);
        setChildren(structure.internalGetChildren());
    }

    @Override
    public String getFilePath() {
        return null;
    }

    @Override
    public String getLabelString() {
        return getName();
    }

    public String getExternalName() {
        return getName();
    }

    @Override
    public boolean isOTP() {
        return true;
    }

    @Override
    public IResource getResource() {
        return null;
    }

    @Override
    public boolean hasIncludes() {
        return true;
    }

    @Override
    public List<IErlElement> internalGetChildren() {
        return super.internalGetChildren();
    }

}
