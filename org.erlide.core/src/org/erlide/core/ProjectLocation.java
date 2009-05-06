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
package org.erlide.core;

import java.util.Collection;
import java.util.EnumSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.runtime.PreferencesUtils;
import org.erlide.runtime.ProjectPreferencesConstants;
import org.osgi.service.prefs.BackingStoreException;

public final class ProjectLocation extends DependencyLocation {
	private IProject project;

	public ProjectLocation(final IProject project) {
		this(project, null);
	}

	public ProjectLocation(final IProject project, final EnumSet<Kind> kind) {
		super(kind);
		Assert.isLegal(project != null,
				"ProjectLocation requires a non-null project");
		this.project = project;
	}

	public IProject getProject() {
		return project;
	}

	@Override
	public void load(final IEclipsePreferences root) {
		final String projectName = root.get(
				ProjectPreferencesConstants.PROJECT, null);
		final IWorkspace workspace = ResourcesPlugin.getWorkspace();
		project = workspace.getRoot().getProject(projectName);
		Assert.isLegal(project != null,
				"ProjectLocation requires a non-null project");
	}

	@Override
	public void store(final IEclipsePreferences root)
			throws BackingStoreException {
		PreferencesUtils.clearAll(root);
		root.put(ProjectPreferencesConstants.PROJECT, project.getName());
	}

	@Override
	public Collection<String> getIncludes() {
		// TODO Auto-generated method stub
		return null; // ErlangCore.getModel().getErlangProject(project.getName()).getProperties();
	}

	@Override
	public Collection<LibraryLocation> getLibraries() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getOutput() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Collection<SourceLocation> getSources() {
		// TODO Auto-generated method stub
		return null;
	}

}
