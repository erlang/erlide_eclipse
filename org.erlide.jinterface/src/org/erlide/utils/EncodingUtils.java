package org.erlide.utils;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

public class EncodingUtils {
    /**
     * Returns the workspace root default charset encoding.
     * 
     * @return the name of the default charset encoding for workspace root.
     * @see IContainer#getDefaultCharset()
     * @see ResourcesPlugin#getEncoding()
     */
    public static String getEncoding() {
        // Verify that workspace is not shutting down (see bug
        // https://bugs.eclipse.org/bugs/show_bug.cgi?id=60687)
        final IWorkspace workspace = getWorkspace();
        if (workspace != null) {
            try {
                return workspace.getRoot().getDefaultCharset();
            } catch (final CoreException e) {
                // fails silently and return plugin global encoding if core
                // exception occurs
            }
        }
        return ResourcesPlugin.getEncoding();
    }

    public static IWorkspace getWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

}
