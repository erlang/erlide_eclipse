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
package org.erlide.core.internal.model.root;

import org.eclipse.core.runtime.IPath;

public abstract class PathEntry {

    protected final IPath path;
    private final String name;
    private final ErlProjectLayout layout;

    public PathEntry(final String name) {
        this(name, null);
    }

    public PathEntry(final String name, final IPath path) {
        this(name, path, null);
    }

    public PathEntry(final String name, final IPath path,
            final ErlProjectLayout layout) {
        this.name = name;
        this.path = path;
        this.layout = layout;
    }

    public String getName() {
        return name;
    }

    public IPath getPath() {
        return path;
    }

    public ErlProjectLayout getLayout() {
        return layout;
    }

    public boolean isRunTime() {
        return true;
    }

    public boolean isCompileTime() {
        return false;
    }

}
