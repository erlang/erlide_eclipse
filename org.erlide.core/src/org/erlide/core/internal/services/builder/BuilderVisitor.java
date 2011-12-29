/*******************************************************************************
 * Copyright (c) 2007, 2010 Vlad Dumitrescu.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package org.erlide.core.internal.services.builder;

import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.builder.BuildResource;
import org.erlide.core.services.builder.BuilderHelper;
import org.erlide.core.services.builder.BuilderHelper.SearchVisitor;
import org.erlide.core.services.builder.MarkerUtils;
import org.erlide.jinterface.ErlLogger;

public class BuilderVisitor implements IResourceDeltaVisitor, IResourceVisitor {

    private final Set<BuildResource> result;
    private final IProgressMonitor monitor;
    private IErlProject erlProject = null;
    private final BuilderHelper helper;

    public BuilderVisitor(final Set<BuildResource> result,
            final IProgressMonitor monitor, final BuilderHelper helper) {
        this.result = result;
        this.monitor = monitor;
        this.helper = helper;
    }

    @Override
    public boolean visit(final IResourceDelta delta) throws CoreException {
        final IResource resource = delta.getResource();
        return visit(resource, delta.getKind(), false);
    }

    @Override
    public boolean visit(final IResource resource) throws CoreException {
        return visit(resource, IResourceDelta.ADDED, true);
    }

    private boolean visit(final IResource resource, final int kind,
            final boolean fullBuild) {
        if (resource.getLocation().toString().contains("lost+found")) {
            return false;
        }
        if (resource.getType() == IResource.PROJECT) {
            erlProject = ErlModelManager.getErlangModel().getErlangProject(
                    (IProject) resource);
            return true;
        }
        if (resource.getType() == IResource.FOLDER) {
            return true;
        }

        final IPath path = resource.getParent().getProjectRelativePath();
        final String ext = resource.getFileExtension();
        if (erlProject.getSourceDirs().contains(path)) {
            if ("erl".equals(ext)) {
                handleErlFile(kind, resource);
                return false;
            }
            if ("yrl".equals(ext)) {
                handleYrlFile(kind, resource);
                return false;
            }
        }
        if (erlProject.getIncludeDirs().contains(path) && "hrl".equals(ext)) {
            try {
                handleHrlFile(kind, resource, fullBuild);
            } catch (final ErlModelException e) {
                ErlLogger.warn(e);
            }
            return false;
        }
        if (erlProject.getOutputLocation().equals(path) && "beam".equals(ext)) {
            try {
                handleBeamFile(kind, resource);
            } catch (final CoreException e) {
                ErlLogger.warn(e);
            }
            return false;
        }
        return true;
    }

    private void handleBeamFile(final int kind, final IResource resource)
            throws CoreException {
        switch (kind) {
        case IResourceDelta.ADDED:
        case IResourceDelta.CHANGED:
            break;
        case IResourceDelta.REMOVED:
            final IResource source = findCorrespondingSource(resource);
            if (source != null) {
                final BuildResource bres = new BuildResource(source);
                result.add(bres);
                monitor.worked(1);
            }
            break;
        }
    }

    public IResource findCorrespondingSource(final IResource beam)
            throws CoreException {
        final String[] p = beam.getName().split("\\.");
        final SearchVisitor searcher = helper.new SearchVisitor(p[0], null);
        beam.getProject().accept(searcher);
        final IResource source = searcher.getResult();
        return source;
    }

    private void handleYrlFile(final int kind, final IResource resource) {
        switch (kind) {
        case IResourceDelta.ADDED:
        case IResourceDelta.CHANGED:
            final BuildResource bres = new BuildResource(resource);
            result.add(bres);
            monitor.worked(1);
            break;

        case IResourceDelta.REMOVED:
            MarkerUtils.deleteMarkers(resource);

            final IPath erl = helper.getErlForYrl(resource);
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

    private void handleHrlFile(final int kind, final IResource resource,
            final boolean fullBuild) throws ErlModelException {
        switch (kind) {
        case IResourceDelta.ADDED:
        case IResourceDelta.REMOVED:
        case IResourceDelta.CHANGED:
            final int n = result.size();
            if (!fullBuild) {
                helper.addDependents(resource, resource.getProject(), result);
            }
            monitor.worked(result.size() - n);
            break;
        }
    }

    private void handleErlFile(final int kind, final IResource resource) {
        switch (kind) {
        case IResourceDelta.ADDED:
        case IResourceDelta.CHANGED:
            final BuildResource bres = new BuildResource(resource);
            result.add(bres);
            monitor.worked(1);
            break;
        case IResourceDelta.REMOVED:
            MarkerUtils.deleteMarkers(resource);
            IPath beam = erlProject.getOutputLocation();
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
                final BuildResource bres2 = new BuildResource(yrl);
                result.add(bres2);
                monitor.worked(1);
            }

            break;
        }
    }

}
