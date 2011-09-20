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
package org.erlide.core.internal.services.builder;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.services.builder.ErlideBuilder;

public class ErlangBuilder extends IncrementalProjectBuilder {
    @SuppressWarnings("rawtypes")
    @Override
    protected IProject[] build(final int kind, final Map args,
            final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        final ErlideBuilder builder = new ErlideBuilder(project);
        return builder.build(kind, args, monitor, getDelta(project));
    }

    @Override
    protected void clean(final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        final ErlideBuilder builder = new ErlideBuilder(project);
        builder.clean(monitor);
    }
}
