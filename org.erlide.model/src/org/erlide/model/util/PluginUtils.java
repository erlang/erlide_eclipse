/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.model.util;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.runtime.IPath;

/**
 * Simple utility functions
 * 
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com] Vlad Jakob C
 */
public class PluginUtils {
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
