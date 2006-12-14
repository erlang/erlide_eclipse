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

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
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
	 * must have the OCaml nature.
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
		final StringBuffer sbResult = new StringBuffer();
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

	public static IResource recursiveFindNamedResource(String name) {
		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace()
				.getRoot();
		try {
			return recursiveFindNamedResource(workspaceRoot, name);
		} catch (final CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	private static IResource recursiveFindNamedResource(IContainer container,
			String name) throws CoreException {
		IResource r = container.findMember(name);
		if (r != null) {
			return r;
		}
		final IResource members[] = container.members();
		for (IResource element : members) {
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
		return ErlangCore.getModel().findErlangProject(project);
	}
}
