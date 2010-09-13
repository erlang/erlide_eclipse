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

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IPath;

public final class ProjectEntry extends PathEntry {
    private IProject project;

    public ProjectEntry(final IProject project) {
        super(null);
        Assert.isLegal(project != null,
                "ProjectLocation requires a non-null project");
        path = project.getLocation();
        this.project = project;
    }

    public IProject getProject() {
        return project;
    }

    public Collection<IPath> getIncludes() {
        return null;
    }

    public Collection<PathEntry> getDependencies() {
        return null;
    }

    public IPath getOutput() {
        return null;
    }

    public Collection<SourceEntry> getSources() {
        return null;
    }

}
