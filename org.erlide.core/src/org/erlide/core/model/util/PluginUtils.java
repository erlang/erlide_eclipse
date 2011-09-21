/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.model.util;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangCore;

/**
 * Simple utility functions
 * 
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com] Vlad Jakob C
 */
public class PluginUtils {
    /**
     * Displays an error that occured during the project creation. *
     * 
     * @param x
     *            details on the error
     * @return IStatus
     */
    public static IStatus makeStatus(final Exception x) {
        return new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID, 0,
                x.getMessage(), x);
    }

    @SuppressWarnings("deprecation")
    public static IPath resolvePVMPath(final IPathVariableManager pvm,
            final IPath path) {
        return pvm.resolvePath(path);
    }

    @SuppressWarnings("deprecation")
    public static IPath getPVMValue(final IPathVariableManager pvm,
            final String name) {
        return pvm.getValue(name);
    }
}
