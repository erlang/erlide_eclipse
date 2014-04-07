package org.erlide.engine.internal.model.root;

import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlLibrary;
import org.erlide.engine.model.root.IErlProject;

public class ErlLibrary extends Openable implements IErlLibrary {

    public ErlLibrary(final String name, final IParent parent) {
        super(parent, name);
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.LIBRARY;
    }

    @Override
    public Collection<IErlProject> getProjects() {
        return null;
    }

    @Override
    public IErlProject getProject(final String name) {
        return null;
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        return false;
    }
}
