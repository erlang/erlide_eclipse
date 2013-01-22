package org.erlide.core.model;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

public interface IBeamLocator {

    public abstract IFile findModuleBeam(final IProject project,
            final String module) throws ErlModelException;

}
