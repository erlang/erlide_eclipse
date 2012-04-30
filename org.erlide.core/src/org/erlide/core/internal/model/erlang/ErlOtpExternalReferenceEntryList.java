package org.erlide.core.internal.model.erlang;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.IBackend;
import org.erlide.core.internal.model.root.Openable;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.util.CoreUtil;
import org.erlide.core.services.search.ErlideOpen;

public class ErlOtpExternalReferenceEntryList extends Openable implements
        IErlExternal {

    public ErlOtpExternalReferenceEntryList(final IParent parent,
            final String name) {
        super(parent, name);
    }

    @Override
    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final IBackend backend = CoreUtil.getBuildOrIdeBackend(getProject()
                .getWorkspaceProject());
        final List<String> libList = ErlideOpen.getLibDirs(backend);
        addExternalEntries(pm, libList, backend);
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm,
            final List<String> libList, final IBackend backend) {
        for (final String libDir : libList) {
            final List<String> srcInclude = ErlideOpen.getLibSrcInclude(
                    backend, libDir);
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

    public boolean hasModuleWithPath(final String path) {
        return false;
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
}
