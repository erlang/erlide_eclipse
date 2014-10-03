package org.erlide.backend.launch;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.backend.api.ErlRuntimeAttributes;

public final class LaunchUtils {

    public static IProject[] getErlangLaunchConfigurationProjects(
            final ILaunchConfiguration configuration) throws CoreException {
        final String projectNamesString = configuration.getAttribute(
                ErlRuntimeAttributes.PROJECTS, "");
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

    private LaunchUtils() {
    }

}
