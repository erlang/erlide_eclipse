package org.erlide.engine.util;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

public final class NatureUtil {

    /**
     * Returns true if the given project is accessible and it has a Erlang
     * nature, otherwise false.
     *
     * @param project
     *            IProject
     * @return boolean
     */
    public static boolean hasErlangNature(final IProject project) {
        if (project != null) {
            try {
                return project.hasNature("org.erlide.core.erlnature");
            } catch (final CoreException e) {
                // project does not exist or is not open
            }
        }
        return false;
    }

    private NatureUtil() {
    }

}
