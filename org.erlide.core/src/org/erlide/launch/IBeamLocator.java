package org.erlide.launch;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.erlide.core.model.root.ErlModelException;

public interface IBeamLocator {

    public abstract IFile findModuleBeam(final IProject project,
            final String module) throws ErlModelException;

}
