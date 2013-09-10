package org.erlide.engine.internal.model;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IBeamLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.SystemConfiguration;

public class BeamLocator implements IBeamLocator {

    @Override
    public IFile findModuleBeam(final IProject project, final String module)
            throws ErlModelException {
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        final IFolder r = project.getFolder(erlProject.getOutputLocation());
        try {
            r.refreshLocal(IResource.DEPTH_ONE, null);
        } catch (final CoreException e) {
            // ignore
        }
        final String beam = SystemConfiguration.withoutExtension(module)
                + ".beam";
        return r.getFile(beam);
    }

}
