/*
 * ResourceUtil.java
 * Created on 2004-08-20
 *
 * cvs-id : $Id$
 */
package org.erlide.core.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.Assert;

/**
 * <p>
 * contains static helping functionality to work on file resources in the
 * workspace.
 * </p>
 * 
 * @author Leif Frenzel
 * @author Andrei Formiga
 */
public class ResourceUtil {

	/**
	 * <p>
	 * the file extension of Erlang source files.
	 * </p>
	 */
	public static final String EXTENSION_ERL = "erl";

	/**
	 * <p>
	 * the file extension of Erlang header files
	 * </p>
	 */
	public static final String EXTENSION_HRL = "hrl";

	/**
	 * <p>
	 * returns whether the passed resource is an OCaml source file, as
	 * recognized by the file extensions '.erl' and '.hrl'.
	 * </p>
	 */
	public static boolean hasErlangExtension(final IResource resource) {
		return has(resource, EXTENSION_ERL) || has(resource, EXTENSION_HRL);
	}

	/**
	 * <p>
	 * returns the output folder of the passed project as resource. The project
	 * must have the Erlang nature.
	 * </p>
	 */
	public static IContainer getOutFolder(final IProject project)
			throws CoreException {
		Assert.isTrue(project.hasNature(ErlangPlugin.NATURE_ID));

		final IPath outputPath = getErlProject(project).getOutputLocation();
		IContainer result;
		if (outputPath.equals(project.getProjectRelativePath())) {
			result = project;
		} else {
			result = project.getFolder(outputPath);
		}
		return result;
	}

	/**
	 * <p>
	 * returns the source folder of the passed project as resource. The project
	 * must have the Erlang nature.
	 * </p>
	 */
	public static IContainer getSourceFolder(final IProject project)
			throws CoreException {
		Assert.isTrue(project.hasNature(ErlangPlugin.NATURE_ID));

		final IContainer result = null;
		// IPath sourcePath = getErlProject(project).getSourcePath();
		// if (sourcePath.equals(project.getProjectRelativePath()))
		// {
		// result = project;
		// } else
		// {
		// result = project.getFolder(sourcePath);
		// }
		return result;
	}

	/**
	 * <p>
	 * reads an input stream and returns the contents as String.
	 * </p>
	 */
	public static String readStream(final InputStream is) throws IOException {
		final StringBuilder sbResult = new StringBuilder();
		final BufferedReader br = new BufferedReader(new InputStreamReader(is));
		String line = br.readLine();
		while (line != null) {
			sbResult.append(line);
			sbResult.append("\n");
			line = br.readLine();
		}
		br.close();
		is.close();

		return sbResult.toString();
	}

	/**
	 * finds the corresponding resource for the specified element. This is
	 * element itself, if it is an IResource, or an adapter. Returns null, if no
	 * resource could be found.
	 */
	public static IResource findResource(final Object element) {
		IResource result = null;
		if (element instanceof IResource) {
			result = (IResource) element;
		} else if (element instanceof IAdaptable) {
			final Object adapter = ((IAdaptable) element)
					.getAdapter(IResource.class);
			if (adapter instanceof IResource) {
				result = (IResource) adapter;
			}
		}
		return result;
	}

	// private static IResource recursiveFindNamedResource(String name) {
	// final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace()
	// .getRoot();
	// try {
	// return recursiveFindNamedResource(workspaceRoot, name);
	// } catch (final CoreException e) {
	// // TODO Auto-generated catch block
	// e.printStackTrace();
	// }
	// return null;
	// }

	private static IResource recursiveFindNamedResource(
			final IContainer container, final String name) throws CoreException {
		if (!container.isAccessible()) {
			return null;
		}
		IResource r = container.findMember(name);
		if (r != null) {
			return r;
		}
		final IResource members[] = container.members();
		for (final IResource element : members) {
			r = element;
			if (r instanceof IContainer) {
				r = recursiveFindNamedResource((IContainer) r, name);
				if (r != null) {
					return r;
				}
			}
		}
		return null;
	}

	public static IResource recursiveFindNamedResourceWithReferences(
			final IContainer container, final String name) throws CoreException {
		final IResource r = recursiveFindNamedResource(container, name);
		if (r != null) {
			return r;
		}
		final IProject project = container.getProject();
		for (final IProject p : project.getReferencedProjects()) {
			final IResource r1 = recursiveFindNamedResource(p, name);
			if (r1 != null) {
				return r1;
			}
		}
		return null;
	}

	private static void recursiveGetAllErlangModules(final List<IFile> result,
			final IContainer container) {
		try {
			for (final IResource r : container.members()) {
				if (r instanceof IContainer) {
					final IContainer c = (IContainer) r;
					if (c.isAccessible()) {
						recursiveGetAllErlangModules(result, c);
					}
				} else if (r instanceof IFile) {
					final IFile f = (IFile) r;
					if (f.getName().endsWith(".erl")) {
						result.add(f);
					}
				}
			}
		} catch (final CoreException e) {
		}
	}

	public static List<String> getAllErlangFiles() {
		final List<IFile> erlangModules = getAllErlangModules();
		final List<String> result = new ArrayList<String>(erlangModules.size());
		for (final IFile f : erlangModules) {
			final String n = f.getName();
			result.add(n.substring(0, n.length() - 4));
		}
		return result;
	}

	public static List<IFile> getAllErlangModules() {
		return getAllErlangModules(ResourcesPlugin.getWorkspace().getRoot());
	}

	public static List<IFile> getAllErlangModules(final IContainer container) {
		final List<IFile> result = new ArrayList<IFile>(100);
		recursiveGetAllErlangModules(result, container);
		return result;
	}

	/**
	 * <p>
	 * returns whether the specified folder is the source folder of its (OCaml)
	 * project.
	 * </p>
	 */
	// public static boolean isSourceFolder(final IFolder folder)
	// {
	// IProject project = folder.getProject();
	// IErlProject erlProject = getErlProject(project);
	// IPath sourcePath = erlProject.getSourcePath();
	// IPath folderPath = folder.getProjectRelativePath();
	// return sourcePath.equals(folderPath);
	// }
	// --- implementation methods
	// -----------------------------------------------
	private static boolean has(final IResource resource, final String extension) {
		final String resExt = resource.getFileExtension();
		return resExt != null && resExt.equalsIgnoreCase(extension);
	}

	private static IErlProject getErlProject(final IProject project) {
		return ErlangCore.getModel().findProject(project);
	}
}
