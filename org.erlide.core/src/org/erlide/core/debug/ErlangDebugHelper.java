package org.erlide.core.debug;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.CoreScope;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;

public class ErlangDebugHelper {

    public void interpret(final IRpcCallSite backend, final IProject project,
            final String moduleName, final boolean distributed,
            final boolean interpret) {
        try {
            final IFile beam = findModuleBeam(project, moduleName);
            if (beam != null) {
                if (beam.exists()) {
                    final String de = interpret ? "" : "de";
                    ErlLogger.debug(de + "interpret " + beam.getLocation());
                    boolean b = ErlideDebug.interpret(backend, beam
                            .getLocation().toString(), distributed, interpret);
                    b = !b;
                } else {
                    ErlLogger.debug("IGNORED MISSING interpret "
                            + (project == null ? "null" : project.getName())
                            + ":" + moduleName);
                }
            } else {
                ErlLogger.debug("IGNORED NULL interpret "
                        + (project == null ? "null" : project.getName()) + ":"
                        + moduleName);
            }
        } catch (final ErlModelException e) {
            ErlLogger.warn(e);
        }
    }

    protected IFile findModuleBeam(final IProject project, final String module)
            throws ErlModelException {
        final IErlProject erlProject = CoreScope.getModel().getErlangProject(
                project);
        final IFolder r = project.getFolder(erlProject.getOutputLocation());
        try {
            r.refreshLocal(IResource.DEPTH_ONE, null);
        } catch (final CoreException e) {
        }
        final String beam = CommonUtils.withoutExtension(module) + ".beam";
        return r.getFile(beam);
    }

}
