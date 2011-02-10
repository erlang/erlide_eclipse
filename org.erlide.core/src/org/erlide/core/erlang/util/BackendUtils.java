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

import org.eclipse.core.resources.IProject;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.runtime.backend.BackendManager;

public class BackendUtils {
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
