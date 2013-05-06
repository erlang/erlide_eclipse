package org.erlide.model.internal.erlang;

import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.model.ErlModelException;
import org.erlide.model.IParent;
import org.erlide.model.ModelPlugin;
import org.erlide.model.internal.root.Openable;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlExternal;
import org.erlide.model.root.IErlExternalRoot;
import org.erlide.model.root.IErlProject;
import org.erlide.model.root.OldErlangProjectProperties;
import org.erlide.model.services.search.ErlideOpen;
import org.erlide.model.util.ModelUtils;
import org.erlide.runtime.api.IRpcSite;

public class ErlOtpExternalReferenceEntryList extends Openable implements
        IErlExternalRoot {

    public ErlOtpExternalReferenceEntryList(final IParent parent,
            final String name) {
        super(parent, name);
    }

    @Override
    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final IErlProject erlProject = ModelUtils.getProject(this);
        final OldErlangProjectProperties properties = new OldErlangProjectProperties(
                erlProject.getWorkspaceProject());
        final IRpcSite backend = ModelPlugin.getDefault().getBackend(
                properties.getRuntimeVersion());
        if (backend != null) {
            final List<String> libList = ErlideOpen.getLibDirs(backend);
            addExternalEntries(pm, libList, backend);
        }
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm,
            final List<String> libList, final IRpcSite backend) {
        final List<List<String>> srcIncludes = ErlideOpen.getLibSrcInclude(
                backend, libList);
        final Iterator<String> iterator = libList.iterator();
        for (final List<String> srcInclude : srcIncludes) {
            final String libDir = iterator.next();
            boolean hasHeaders = false;
            for (final String path : srcInclude) {
                if (includePath(path)) {
                    hasHeaders = true;
                    break;
                }
            }
            final IErlExternal external = new ErlExternalReferenceEntry(this,
                    getLibName(libDir), libDir, true, hasHeaders);
            addChild(external);
            for (final String i : srcInclude) {
                external.addChild(new ErlExternalReferenceEntry(external,
                        getLibName(i), i, false, includePath(i)));
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
        if (s.equals("ebin")) {
            s = p.removeLastSegments(1).lastSegment();
        }
        final int dashPos = s.lastIndexOf('-');
        if (dashPos != -1) {
            return s.substring(0, dashPos);
        }
        return s;
    }

    @Override
    public boolean isOpen() {
        return super.isOpen();
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

    @Override
    public void removeExternal() {
    }
}
