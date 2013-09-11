package org.erlide.engine.model;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.erlide.engine.services.ErlangService;

public interface IBeamLocator extends ErlangService {

    IFile findModuleBeam(final IProject project, final String module)
            throws ErlModelException;

}
