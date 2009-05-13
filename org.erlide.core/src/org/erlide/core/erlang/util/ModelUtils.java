package org.erlide.core.erlang.util;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlPreprocessorDef;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.internal.ErlModelManager;
import org.erlide.core.preferences.ErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideOpen;

public class ModelUtils {

	/**
	 * Try to find include file, by searching include paths in the project
	 * (replacing with path variables if needed). If the file is not in the
	 * include paths, the original path is returned
	 * 
	 * @param project
	 *            the project with include dirs
	 * @param filePath
	 *            the path to the include file
	 * @param externalIncludes
	 *            TODO
	 * @return the path to the include file
	 */
	public static String findIncludeFile(final IProject project,
			final String filePath, final String externalIncludes) {
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		final ErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		for (final String includeDir : prefs.getIncludeDirs()) {
			IPath p = new Path(includeDir).append(filePath);
			p = pvm.resolvePath(p);
			final File f = new File(p.toOSString());
			if (f.exists()) {
				return p.toString();
			}
		}
		final String s = ErlideOpen.getExternalInclude(ErlangCore
				.getBackendManager().getIdeBackend(), filePath,
				externalIncludes, ErlangCore.getModel().getPathVars());
		if (s != null) {
			return s;
		}
		return filePath;
	}

	public static IErlPreprocessorDef findPreprocessorDef(final Backend b,
			final Collection<IProject> projects, final String moduleName,
			final String definedName, final IErlElement.Kind type,
			final String externalIncludes) {
		try {
			final List<IErlModule> modulesDone = new ArrayList<IErlModule>();
			final IErlModel model = ErlModelManager.getDefault()
					.getErlangModel();
			for (final IProject project : projects) {
				final IErlProject p = model.findProject(project);
				if (p != null) {
					final IErlModule m = p.getModule(moduleName);
					if (m != null) {
						final IErlPreprocessorDef def = findPreprocessorDef(b,
								projects, m, definedName, type,
								externalIncludes, modulesDone);
						if (def != null) {
							return def;
						}
					}
				}
			}
		} catch (final CoreException e) {
		}
		return null;
	}

	/**
	 * @param project
	 * @param m
	 * @param definedName
	 * @param type
	 * @param externalIncludes
	 *            TODO
	 * @param modulesDone
	 * @return
	 * @throws CoreException
	 */
	private static IErlPreprocessorDef findPreprocessorDef(final Backend b,
			final Collection<IProject> projects, IErlModule m,
			final String definedName, final IErlElement.Kind type,
			final String externalIncludes, final List<IErlModule> modulesDone)
			throws CoreException {
		if (m == null) {
			return null;
		}
		modulesDone.add(m);
		m.open(null);
		final IErlPreprocessorDef pd = m.findPreprocessorDef(definedName, type);
		if (pd != null) {
			return pd;
		}
		final Collection<ErlangIncludeFile> includes = m.getIncludedFiles();
		for (final ErlangIncludeFile element : includes) {
			IResource re = null;
			IProject project = null;
			for (final IProject p : projects) {
				re = ResourceUtil.recursiveFindNamedResourceWithReferences(p,
						element.getFilenameLastPart());
				if (re != null) {
					project = p;
					break;
				}
			}
			if (re == null) {
				try {
					String s = element.getFilename();
					if (element.isSystemInclude()) {
						s = ErlideOpen.getIncludeLib(b, s);
					} else {
						s = findIncludeFile(project, s, externalIncludes);
					}
					re = ResourceUtil.recursiveFindNamedResourceWithReferences(
							project, s);
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
			}
			if (re != null && re instanceof IFile) {
				m = ErlModelManager.getDefault().getErlangModel().getModule(
						(IFile) re);
				if (m != null && !modulesDone.contains(m)) {
					final IErlPreprocessorDef pd2 = findPreprocessorDef(b,
							projects, m, definedName, type, externalIncludes,
							modulesDone);
					if (pd2 != null) {
						return pd2;
					}
				}
			}
		}
		return null;
	}

}
