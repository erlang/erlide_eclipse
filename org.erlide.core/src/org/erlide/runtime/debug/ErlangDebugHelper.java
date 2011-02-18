package org.erlide.runtime.debug;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.backend.ErlCallable;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideDebug;

public class ErlangDebugHelper {

    public void interpret(final ErlCallable backend, final IProject project,
            final String moduleName, final boolean distributed,
            final boolean interpret) {
        try {
            final IFile beam = findModuleBeam(project, moduleName);
            if (beam != null && beam.exists()) {
                final String de = interpret ? "" : "de";
                ErlLogger.debug(de + "interpret " + beam.getLocation());
                boolean b = ErlideDebug.interpret(backend, beam.getLocation()
                        .toString(), distributed, interpret);
                b = !b;
            } else {
                ErlLogger.debug("IGNORED MISSING interpret "
                        + (project == null ? "null" : project.getName()) + ":"
                        + moduleName);
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
    }

    protected IFile findModuleBeam(final IProject project, final String module)
            throws ErlModelException {
        final IErlProject erlProject = ErlangCore.getModel().getErlangProject(
                project);
        final IFolder r = project.getFolder(erlProject.getOutputLocation());
        try {
            r.refreshLocal(IResource.DEPTH_ONE, null);
        } catch (final CoreException e) {
        }
        final String beam = ErlideUtil.withoutExtension(module) + ".beam";
        return r.getFile(beam);
    }

}
