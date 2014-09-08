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
package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.root.IErlProject;

public abstract class ErlangBuilder {

    public abstract IProject[] build(final BuildKind kind, final IErlProject erlProject,
            final BuildNotifier notifier) throws CoreException;

    public abstract void clean(IErlProject erlProject, final BuildNotifier notifier);

    public abstract BuilderProperties getProperties();

    public enum BuildKind {
        NONE, AUTO, FULL, INCREMENTAL, CLEAN;

        public static BuildKind get(final int kind) {
            switch (kind) {
            case IncrementalProjectBuilder.AUTO_BUILD:
                return AUTO;
            case IncrementalProjectBuilder.CLEAN_BUILD:
                return CLEAN;
            case IncrementalProjectBuilder.FULL_BUILD:
                return FULL;
            case IncrementalProjectBuilder.INCREMENTAL_BUILD:
                return INCREMENTAL;
            default:
                return NONE;
            }
        }
    }

}
