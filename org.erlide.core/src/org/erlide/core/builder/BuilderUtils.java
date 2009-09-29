/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.OldErlangProjectProperties;

public final class BuilderUtils {
	private BuilderUtils() {
	}

	public static boolean isDebugging() {
		if (ErlangPlugin.getDefault() == null) {
			return false;
		}
		return ErlangPlugin.getDefault().isDebugging()
				&& Platform.getDebugOption("org.erlide.core/debug/builder")
						.equals("true");
	}

	/**
	 * @param project
	 * @param prefs
	 * @return
	 */
	public static List<String> getIncludeDirs(final IProject project,
			final List<String> includeDirs) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final String[] incs = prefs.getIncludeDirs();
		final IPathVariableManager pvm = ResourcesPlugin.getWorkspace()
				.getPathVariableManager();
		for (int i = 0; i < incs.length; i++) {
			final IPath inc = pvm.resolvePath(new Path(incs[i]));
			if (inc.isAbsolute()) {
				includeDirs.add(inc.toString());
			} else {
				final IFolder folder = project.getFolder(incs[i]);
				if (folder != null) {
					final IPath location = folder.getLocation();
					includeDirs.add(location.toString());
				}
			}
		}
		return includeDirs;
	}

	public static boolean isInteresting(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);

		List<String> interestingPaths = new ArrayList<String>();
		final String[] srcs = prefs.getSourceDirs();
		for (String s : srcs) {
			interestingPaths.add(s);
		}
		String[] incs = prefs.getIncludeDirs();
		for (String s : incs) {
			interestingPaths.add(s);
		}
		interestingPaths.add(prefs.getOutputDir());

		final IPath projectPath = project.getFullPath();
		final IPath fullPath = resource.getFullPath();
		for (final String element : interestingPaths) {
			final IPath sp = projectPath.append(new Path(element));
			if (fullPath.isPrefixOf(sp)) {
				return true;
			}
		}
		return false;
	}

	public static boolean isInCodePath(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final IPath projectPath = project.getFullPath();
		final String[] srcs = prefs.getSourceDirs();
		final IPath exceptLastSegment = resource.getFullPath()
				.removeLastSegments(1);
		for (final String element : srcs) {
			final IPath sp = projectPath.append(new Path(element));
			if (sp.equals(exceptLastSegment)) {
				return true;
			}
		}

		return false;
	}

	public static boolean isInIncludedPath(final IResource resource,
			final IProject my_project) {
		final List<String> inc = new ArrayList<String>();
		getIncludeDirs(my_project, inc);

		for (final String s : inc) {
			final IPath p = new Path(s);
			final IPath resourcePath = resource.getLocation();
			if (p.isPrefixOf(resourcePath)) {
				return true;
			}
		}
		return false;
	}

	public static boolean isInOutputPath(final IResource resource,
			final IProject project) {
		final OldErlangProjectProperties prefs = ErlangCore
				.getProjectProperties(project);
		final IPath projectPath = project.getLocation();

		final String out = prefs.getOutputDir();
		return projectPath.append(new Path(out)).isPrefixOf(
				resource.getLocation());
	}

}
