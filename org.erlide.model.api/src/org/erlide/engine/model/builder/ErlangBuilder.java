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

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;

public abstract class ErlangBuilder extends IncrementalProjectBuilder {

    public abstract String getId();

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IProgressMonitor monitor) throws CoreException {

        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return null;
        }
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);

        if (!validateBuildConfiguration(erlProject)) {
            final BuilderConfigType config = erlProject.getBuilderConfigType();
            final BuilderTool tool = erlProject.getBuilderProperties().getBuilderTool();
            ErlLogger.warn("Builder tool and config mismatch: " + tool + " " + config);

            monitor.setCanceled(true);
        }

        return null;
    }

    private boolean validateBuildConfiguration(final IErlProject erlProject) {
        final BuilderConfigType config = erlProject.getBuilderConfigType();
        final BuilderTool tool = erlProject.getBuilderProperties().getBuilderTool();
        if (!config.matchTool(tool)) {
            final String msg = String.format(
                    "Project's builder tool %s and configuration %s don't match", tool,
                    config);
            MarkerUtils.addProblemMarker(erlProject.getWorkspaceProject(), null, null,
                    msg, 0, IMarker.SEVERITY_WARNING);
            return false;
        }
        return true;
    }

    public static IErlangBuilderFactory getFactory() {
        return ExtensionUtils.getSingletonExtension(
                "org.erlide.model.api.builderFactory", IErlangBuilderFactory.class);
    }

    public abstract BuilderProperties getProperties();

}
