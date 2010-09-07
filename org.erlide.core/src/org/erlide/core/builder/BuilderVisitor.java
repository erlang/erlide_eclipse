/*******************************************************************************
 * Copyright (c) 2007, 2010 Vlad Dumitrescu.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package org.erlide.core.builder;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.builder.BuilderUtils.SearchVisitor;
import org.erlide.core.builder.internal.MarkerHelper;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.util.ErlLogger;

class BuilderVisitor implements IResourceDeltaVisitor, IResourceVisitor {
	private enum ResourceCategory {
		NONE, ERL, HRL, YRL, BEAM;
	}

	private enum FolderCategory {
		NONE, SOURCE, INCLUDE, OUTPUT;
	}

	private final Set<BuildResource> result;
	private final IProgressMonitor monitor;
	private OldErlangProjectProperties prefs = null;
	private FolderCategory folderCategory = FolderCategory.NONE;
	private ResourceCategory resourceCategory;

	public BuilderVisitor(final Set<BuildResource> result,
			final IProgressMonitor monitor) {
		this.result = result;
		this.monitor = monitor;
		System.out.println("-------------------");
	}

	public boolean visit(final IResourceDelta delta) throws CoreException {
		final IResource resource = delta.getResource();
		return visit(resource, delta.getKind());
	}

	public boolean visit(final IResource resource) throws CoreException {
		return visit(resource, IResourceDelta.CHANGED);
	}

	private boolean visit(IResource resource, int kind) {
		final IProject my_project = resource.getProject();

		// return true to continue visiting children.
		if (resource.getType() == IResource.PROJECT) {
			prefs = ErlangCore.getProjectProperties(my_project);
			return true;
		}
		if (resource.getType() == IResource.FOLDER) {
			return true;
		}

		folderCategory = categorizeFolder(resource.getParent());
		if (folderCategory == FolderCategory.NONE) {
			return true;
		}
		resourceCategory = categorizeResource(resource);
		return handleFile(kind, resource);
	}

	private boolean handleFile(int kind, IResource resource) {
		System.out.println(" ## " + resource + " " + kind + " : "
				+ folderCategory + "," + resourceCategory);

		try {
			switch (resourceCategory) {
			case ERL:
				handleErlFile(kind, resource);
				break;
			case HRL:
				handleHrlFile(kind, resource);
				break;
			case YRL:
				handleYrlFile(kind, resource);
				break;
			case BEAM:
				handleBeamFile(kind, resource);
				break;
			default:
			}
		} catch (Exception e) {
			// ignore
		}
		return false;
	}

	private FolderCategory categorizeFolder(IResource resource) {
		IPath path = resource.getProjectRelativePath();
		if (prefs.getSourceDirs().contains(path)) {
			return FolderCategory.SOURCE;
		}
		if (prefs.getIncludeDirs().contains(path)) {
			return FolderCategory.INCLUDE;
		}
		if (prefs.getOutputDir().equals(path)) {
			return FolderCategory.OUTPUT;
		}
		return FolderCategory.NONE;
	}

	private BuilderVisitor.ResourceCategory categorizeResource(
			IResource resource) {
		String ext = resource.getFileExtension();
		if ("erl".equals(ext)) {
			return ResourceCategory.ERL;
		}
		if ("hrl".equals(ext)) {
			return ResourceCategory.HRL;
		}
		if ("beam".equals(ext)) {
			return ResourceCategory.BEAM;
		}
		if ("yrl".equals(ext)) {
			return ResourceCategory.YRL;
		}
		return ResourceCategory.NONE;
	}

	private void handleBeamFile(final int kind, final IResource resource)
			throws CoreException {
		if (folderCategory != FolderCategory.OUTPUT) {
			return;
		}
		switch (kind) {
		case IResourceDelta.ADDED:
		case IResourceDelta.CHANGED:
			break;
		case IResourceDelta.REMOVED:
			final String[] p = resource.getName().split("\\.");
			final SearchVisitor searcher = new SearchVisitor(p[0], null);
			resource.getProject().accept(searcher);
			if (searcher.fResult != null) {
				final BuildResource bres = new BuildResource(searcher.fResult);
				result.add(bres);
				monitor.worked(1);
			}
			break;
		}
	}

	private void handleYrlFile(final int kind, final IResource resource) {
		if (folderCategory != FolderCategory.SOURCE) {
			return;
		}
		switch (kind) {
		case IResourceDelta.ADDED:
		case IResourceDelta.CHANGED:
			final BuildResource bres = new BuildResource(resource);
			result.add(bres);
			monitor.worked(1);
			break;

		case IResourceDelta.REMOVED:
			MarkerHelper.deleteMarkers(resource);

			IPath erl = BuilderUtils.getErlForYrl(resource);
			final IResource br = resource.getProject().findMember(erl);
			if (br != null) {
				try {
					br.delete(true, null);
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
				monitor.worked(1);
			}
			break;
		}
	}

	private void handleHrlFile(final int kind, final IResource resource)
			throws ErlModelException {
		if (folderCategory != FolderCategory.INCLUDE) {
			return;
		}
		switch (kind) {
		case IResourceDelta.ADDED:
		case IResourceDelta.REMOVED:
		case IResourceDelta.CHANGED:
			final int n = result.size();
			BuilderUtils.addDependents(resource, resource.getProject(), result);
			monitor.worked(result.size() - n);
			break;
		}
	}

	private void handleErlFile(final int kind, final IResource resource) {
		if (folderCategory != FolderCategory.SOURCE) {
			return;
		}
		switch (kind) {
		case IResourceDelta.ADDED:
		case IResourceDelta.CHANGED:
			// handle changed resource
			final BuildResource bres = new BuildResource(resource);
			result.add(bres);
			monitor.worked(1);
			break;
		case IResourceDelta.REMOVED:
			// handle removed resource
			MarkerHelper.deleteMarkers(resource);

			IPath beam = prefs.getOutputDir();
			final IPath module = beam.append(resource.getName())
					.removeFileExtension();
			beam = module.addFileExtension("beam").setDevice(null);
			final IResource br = resource.getProject().findMember(beam);
			if (br != null) {
				try {
					br.delete(true, null);
				} catch (final Exception e) {
					ErlLogger.warn(e);
				}
			}

			// was it derived from a yrl?
			final IPath yrlpath = resource.getProjectRelativePath()
					.removeFileExtension().addFileExtension("yrl");
			final IResource yrl = resource.getProject().findMember(yrlpath);
			if (yrl != null) {
				final BuildResource bres2 = new BuildResource(resource);
				result.add(bres2);
				monitor.worked(1);
			}

			break;
		}
	}

}