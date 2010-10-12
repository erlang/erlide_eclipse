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
package org.erlide.core.erlang.util;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.runtime.backend.BackendManager;

public class BackendUtils {
    public static IProject[] getProjects(final String attribute) {
        final String[] projectNames = attribute.split(";");
        return getProjects(projectNames);
    }

    public static IProject[] getProjects(final String[] projectNames) {
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

    public static String getErlideNameTag() {
        String fUniqueId;
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final String location = root.getLocation().toPortableString();
        final String user = System.getProperty("user.name");
        final String timestamp = Long
                .toHexString(System.currentTimeMillis() & 0xFFFFFF);
        fUniqueId = Long.toHexString(location.hashCode() & 0xFFFFF) + "_"
                + user + "_" + timestamp;
        return fUniqueId.replaceAll("[^a-zA-Z0-9_-]", "");
    }

    public static Backend getBuildOrIdeBackend(final IProject project) {
        final BackendManager backendManager = ErlangCore.getBackendManager();
        if (project != null) {
            try {
                return backendManager.getBuildBackend(project);
            } catch (final BackendException e) {
            }
        }
        return backendManager.getIdeBackend();
    }

}
