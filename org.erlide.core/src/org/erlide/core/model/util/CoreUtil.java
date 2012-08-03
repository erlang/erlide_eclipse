/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.model.util;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendManager;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.utils.Util;

public final class CoreUtil {
    /**
     * Returns the given file's contents as a byte array.
     * 
     * @throws CoreException
     * @throws IOException
     */
    public static byte[] getResourceContentsAsByteArray(final IFile file)
            throws CoreException, IOException {
        InputStream stream = null;
        stream = new BufferedInputStream(file.getContents(true));
        try {
            return Util.getInputStreamAsByteArray(stream, -1);
        } finally {
            try {
                stream.close();
            } catch (final IOException e) {
                // ignore
            }
        }
    }

    /**
     * Returns the given file's contents as a character array.
     * 
     * @throws CoreException
     * @throws IOException
     * 
     * @throws ErlModelException
     */
    public static char[] getResourceContentsAsCharArray(final IFile file)
            throws IOException, CoreException {
        // Get encoding from file
        Charset encoding = null;
        try {
            encoding = Charset.forName(file.getCharset());
        } catch (final CoreException ce) {
            // do not use any encoding
        }
        return getResourceContentsAsCharArray(file, encoding);
    }

    public static char[] getResourceContentsAsCharArray(final IFile file,
            final Charset encoding) throws IOException, CoreException {
        // Get resource contents
        InputStream stream = null;
        stream = new BufferedInputStream(file.getContents(true));
        try {
            return Util.getInputStreamAsCharArray(stream, -1, encoding);
        } finally {
            try {
                stream.close();
            } catch (final IOException e) {
                // ignore
            }
        }
    }

    /*
     * Returns the index of the most specific argument paths which is strictly
     * enclosing the path to check
     */
    public static int indexOfEnclosingPath(final IPath checkedPath,
            final IPath[] paths, final int pathCount) {

        int bestMatch = -1, bestLength = -1;
        for (int i = 0; i < pathCount; i++) {
            if (paths[i].equals(checkedPath)) {
                continue;
            }
            if (paths[i].isPrefixOf(checkedPath)) {
                final int currentLength = paths[i].segmentCount();
                if (currentLength > bestLength) {
                    bestLength = currentLength;
                    bestMatch = i;
                }
            }
        }
        return bestMatch;
    }

    /*
     * Returns the index of the first argument paths which is equal to the path
     * to check
     */
    public static int indexOfMatchingPath(final IPath checkedPath,
            final IPath[] paths, final int pathCount) {

        for (int i = 0; i < pathCount; i++) {
            if (paths[i].equals(checkedPath)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Returns the index of the first argument paths which is strictly nested
     * inside the path to check
     */
    public static int indexOfNestedPath(final IPath checkedPath,
            final IPath[] paths, final int pathCount) {

        for (int i = 0; i < pathCount; i++) {
            if (checkedPath.equals(paths[i])) {
                continue;
            }
            if (checkedPath.isPrefixOf(paths[i])) {
                return i;
            }
        }
        return -1;
    }

    private CoreUtil() {
    }

    public static IBackend getBuildOrIdeBackend(final IProject project) {
        final IBackendManager backendManager = BackendCore.getBackendManager();
        if (project != null) {
            try {
                return backendManager.getBuildBackend(project);
            } catch (final BackendException e) {
            }
        }
        return backendManager.getIdeBackend();
    }

    public static IProject[] getErlangLaunchConfigurationProjects(
            final ILaunchConfiguration configuration) throws CoreException {
        final String projectNamesString = configuration.getAttribute(
                ErlLaunchAttributes.PROJECTS, "");
        final String[] projectNames = projectNamesString.split(";");
        final List<IProject> projects = new ArrayList<IProject>();
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        for (final String s : projectNames) {
            if (s != null && s.length() > 0) {
                final IProject p = root.getProject(s);
                if (p != null) {
                    projects.add(p);
                }
            }
        }
        return projects.toArray(new IProject[projects.size()]);
    }

}
