package org.erlide.core.internal.model.erlang;

import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.IBackend;
import org.erlide.core.internal.model.root.Openable;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlExternal;
import org.erlide.core.model.root.IParent;
import org.erlide.core.model.util.CoreUtil;
import org.erlide.core.services.search.ErlideOpen;

import com.google.common.collect.Lists;

public class ErlExternalReferenceEntry extends Openable implements IErlExternal {

    private final String entry;
    private final boolean prebuilt;
    private final boolean hasHeaders;

    protected ErlExternalReferenceEntry(final IParent parent,
            final String name, final String entry, final boolean prebuilt,
            final boolean hasHeaders) {
        super(parent, name);
        this.entry = entry;
        this.prebuilt = prebuilt;
        this.hasHeaders = hasHeaders;
    }

    @Override
    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        if (prebuilt) {
            // already done
            return true;
        }
        final IBackend backend = CoreUtil.getBuildOrIdeBackend(getProject()
                .getWorkspaceProject());
        if (backend != null) {
            final List<String> files = ErlideOpen.getLibFiles(backend, entry);
            final List<IErlModule> children = Lists
                    .newArrayListWithCapacity(files.size());
            for (final String file : files) {
                children.add(new ErlModule(this, getName(file), null, null,
                        file, false));
            }
            setChildren(children);
            return true;
        }
        return false;
    }

    private String getName(final String file) {
        final IPath p = new Path(file);
        return p.lastSegment();
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

    @Override
    public boolean isOTP() {
        final IParent parent = getParent();
        if (parent instanceof IErlExternal) {
            final IErlExternal external = (IErlExternal) parent;
            return external.isOTP();
        }
        return false;
    }

    @Override
    public IResource getResource() {
        return null;
    }

    @Override
    public boolean hasIncludes() {
        return hasHeaders;
    }
}
