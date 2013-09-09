package org.erlide.engine.model;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

public interface IBeamLocator {

    IFile findModuleBeam(final IProject project, final String module)
            throws ErlModelException;

}
