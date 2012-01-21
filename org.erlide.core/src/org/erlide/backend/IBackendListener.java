/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend;

import org.eclipse.core.resources.IProject;

public interface IBackendListener {

    public void runtimeAdded(IBackend backend);

    public void runtimeRemoved(IBackend backend);

    public void moduleLoaded(IBackend aBackend, IProject project,
            String moduleName);

}
