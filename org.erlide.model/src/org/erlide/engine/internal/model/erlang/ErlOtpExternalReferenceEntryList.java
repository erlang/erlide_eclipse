package org.erlide.engine.internal.model.erlang;

import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.Openable;
import org.erlide.engine.internal.util.BackendUtil;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlProject;
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
        final Map<String, List<String>> srcIncludes = ErlangEngine
                .getInstance().getOpenService().getOtpLibSrcIncludes(backend);
        for (final String srcInclude : srcIncludes.keySet()) {
            boolean hasHeaders = false;
            final List<String> paths = srcIncludes.get(srcInclude);
            for (final String path : paths) {
                if (includePath(path)) {
                    hasHeaders = true;
                    break;
                }
            }
            final IErlExternal external = new ErlExternalReferenceEntry(this,
                    getLibName(srcInclude), srcInclude, true, hasHeaders);
            addChild(external);
            for (final String path : paths) {
                external.addChild(new ErlExternalReferenceEntry(external,
                        getLibName(path), path, false, includePath(path)));
            }
        }
    }

    private final boolean includePath(final String path) {
        final IPath p = new Path(path);
        return p.lastSegment().equals("include");
    }

    private String getLibName(final String libDir) {
        final IPath p = new Path(libDir);
        String s = p.lastSegment();
        if ("ebin".equals(s)) {
            s = p.removeLastSegments(1).lastSegment();
        }
        final int dashPos = s.lastIndexOf('-');
        if (dashPos != -1) {
            return s.substring(0, dashPos);
        }
        return s;
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
