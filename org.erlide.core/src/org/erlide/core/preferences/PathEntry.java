/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.preferences;

import org.eclipse.core.runtime.IPath;

public abstract class PathEntry {

    protected IPath path;

    public PathEntry(IPath path) {
        this.path = path;
    }

    public IPath getPath() {
        return path;
    }

    public boolean isRunTime() {
        return true;
    }

    public boolean isCompileTime() {
        return false;
    }

}
