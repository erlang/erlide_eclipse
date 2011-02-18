package org.erlide.core.erlang.internal;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.Backend;
import org.erlide.backend.ErlCallable;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IParent;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideOpen;

public class ErlOtpExternalReferenceEntryList extends Openable implements
        IErlExternal {

    private final Backend backend;

    public ErlOtpExternalReferenceEntryList(final IParent parent,
            final String name, final Backend backend) {
        super(parent, name);
        this.backend = backend;
    }

    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        ErlLogger.debug(
                "ErlOtpExternalReferenceEntryList#buildStructure %s %s",
                backend.getJavaNodeName(), backend.getName());
        final List<String> libList = ErlideOpen.getLibDirs(backend);
        addExternalEntries(pm, libList);
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm,
            final List<String> libList) {
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
    protected void closing(final Object info) throws ErlModelException {
        // TODO Auto-generated method stub

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

    public boolean isRoot() {
        return true;
    }

    public ErlCallable getBackend() {
        return backend;
    }

    public boolean isOTP() {
        return true;
    }

    @Override
    public IResource getResource() {
        return null;
    }

    public boolean hasHeaders() {
        return true;
    }
}
