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
package org.erlide.engine.model.builder;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.util.services.ExtensionUtils;

public abstract class ErlangBuilder extends IncrementalProjectBuilder {

    private ProjectConfigurationPersister configurator;

    public abstract String getId();

    @Override
    public abstract IProject[] build(int kind, Map<String, String> args,
            IProgressMonitor monitor) throws CoreException;

    ProjectConfigurationPersister getConfigurationPersister() {
        return configurator;
    }

    public void setConfigurationPersister(
            final ProjectConfigurationPersister configurator) {
        this.configurator = configurator;
    }

    public static IErlangBuilderFactory getFactory() {
        return ExtensionUtils.getSingletonExtension(
                "org.erlide.model.api.builderFactory",
                IErlangBuilderFactory.class);
    }

}
