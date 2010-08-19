package org.erlide.runtime.launch;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideDebug;

public class ErlangDebugHelper {

	public void interpret(final Backend backend, final String project,
			final String module, final boolean distributed,
			final boolean interpret) {
		try {
			IFile beam = findModuleBeam(project, module);
			if (beam!=null && beam.exists()) {
				final String de = interpret ? "" : "de";
				ErlLogger.debug(de + "interpret " + beam.getLocation());
				ErlideDebug.interpret(backend, beam.getLocation().toString(),
						distributed, interpret);
			} else {
				ErlLogger.debug("IGNORED MISSING interpret " + project + ":"
						+ module);
			}
		} catch (final ErlModelException e) {
			ErlLogger.warn(e);
		}
	}

	protected IFile findModuleBeam(String project, String module)
			throws ErlModelException {
		final IErlProject eprj = ErlangCore.getModel()
				.getErlangProject(project);
		final IProject iprj = eprj.getProject();
		final IFolder r = iprj.getFolder(eprj.getOutputLocation());
		try {
			r.refreshLocal(IResource.DEPTH_ONE, null);
		} catch (CoreException e) {
		}
		final String beam = ErlideUtil.withoutExtension(module) + ".beam";
		return r.getFile(beam);
	}

}
