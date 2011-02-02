package org.erlide.core.erlang.internal;

import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.jinterface.backend.Backend;

import com.google.common.collect.Lists;

import erlang.ErlideOpen;

public class ErlExternalReferenceEntry extends Openable implements IErlExternal {

    private final String entry;
    private final boolean prebuilt;

    protected ErlExternalReferenceEntry(final IParent parent,
            final String name, final String entry, final boolean prebuilt) {
        super(parent, name);
        this.entry = entry;
        this.prebuilt = prebuilt;
    }

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
        final Backend backend = getBackend();
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

    public Backend getBackend() {
        final IParent parent = getParent();
        if (parent instanceof IErlExternal) {
            final IErlExternal external = (IErlExternal) parent;
            return external.getBackend();
        }
        return null;
    }

    private String getName(final String file) {
        final IPath p = new Path(file);
        return p.lastSegment();
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

    public boolean isOTP() {
        return false;
    }

}
