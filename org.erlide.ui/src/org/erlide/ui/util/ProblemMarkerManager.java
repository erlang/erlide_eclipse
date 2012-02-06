/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util;

import java.util.HashSet;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.text.source.AnnotationModelEvent;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelListener;
import org.eclipse.jface.text.source.IAnnotationModelListenerExtension;
import org.eclipse.swt.widgets.Display;
import org.erlide.jinterface.ErlLogger;

/**
 * Listens to resource deltas and filters for marker changes of type
 * IMarker.PROBLEM Viewers showing error ticks should register as listener to
 * this type.
 */
public class ProblemMarkerManager implements IResourceChangeListener,
        IAnnotationModelListener, IAnnotationModelListenerExtension {

    /**
     * Visitors used to look if the element change delta contains a marker
     * change.
     */
    private static class ProjectErrorVisitor implements IResourceDeltaVisitor {

        private final HashSet<IResource> fChangedElements;

        public ProjectErrorVisitor(final HashSet<IResource> changedElements) {
            fChangedElements = changedElements;
        }

        @Override
        public boolean visit(final IResourceDelta delta) throws CoreException {
            final IResource res = delta.getResource();
            if (res instanceof IProject
                    && delta.getKind() == IResourceDelta.CHANGED) {
                final IProject project = (IProject) res;
                if (!project.isAccessible()) {
                    // only track open Erlang projects
                    return false;
                }
            }
            checkInvalidate(delta, res);
            return true;
        }

        private void checkInvalidate(final IResourceDelta delta,
                IResource resource) {
            final int kind = delta.getKind();
            if ((kind == IResourceDelta.REMOVED || kind == IResourceDelta.ADDED || kind == IResourceDelta.CHANGED)
                    && isErrorDelta(delta)) {
                // invalidate the resource and all parents
                while (resource.getType() != IResource.ROOT
                        && fChangedElements.add(resource)) {
                    resource = resource.getParent();
                }
            }
        }

        private boolean isErrorDelta(final IResourceDelta delta) {
            if ((delta.getFlags() & IResourceDelta.MARKERS) != 0) {
                final IMarkerDelta[] markerDeltas = delta.getMarkerDeltas();
                for (int i = 0; i < markerDeltas.length; i++) {
                    if (markerDeltas[i].isSubtypeOf(IMarker.PROBLEM)) {
                        final int kind = markerDeltas[i].getKind();
                        if (kind == IResourceDelta.ADDED
                                || kind == IResourceDelta.REMOVED) {
                            return true;
                        }
                        final int severity = markerDeltas[i].getAttribute(
                                IMarker.SEVERITY, -1);
                        final int newSeverity = markerDeltas[i].getMarker()
                                .getAttribute(IMarker.SEVERITY, -1);
                        if (newSeverity != severity) {
                            return true;
                        }
                    }
                }
            }
            return false;
        }
    }

    private final ListenerList fListeners;

    public ProblemMarkerManager() {
        fListeners = new ListenerList();
    }

    /*
     * @see IResourceChangeListener#resourceChanged
     */
    @Override
    public void resourceChanged(final IResourceChangeEvent event) {
        final HashSet<IResource> changedElements = new HashSet<IResource>();

        try {
            final IResourceDelta delta = event.getDelta();
            if (delta != null) {
                delta.accept(new ProjectErrorVisitor(changedElements));
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }

        if (!changedElements.isEmpty()) {
            final IResource[] changes = changedElements
                    .toArray(new IResource[changedElements.size()]);
            fireChanges(changes, true);
        }
    }

    @Override
    public void modelChanged(final IAnnotationModel model) {
        // no action
    }

    @Override
    public void modelChanged(final AnnotationModelEvent event) {
        if (event instanceof ErlangModuleAnnotationModelEvent) {
            final ErlangModuleAnnotationModelEvent emEvent = (ErlangModuleAnnotationModelEvent) event;
            if (emEvent.includesProblemMarkerAnnotationChanges()) {
                final IResource[] changes = new IResource[] { emEvent
                        .getUnderlyingResource() };
                fireChanges(changes, false);
            }
        }
    }

    /**
     * Adds a listener for problem marker changes.
     */
    public void addListener(final IProblemChangedListener listener) {
        if (fListeners.isEmpty()) {
            ResourcesPlugin.getWorkspace().addResourceChangeListener(this);
            // JavaPlugin.getDefault().getCompilationUnitDocumentProvider()
            // .addGlobalAnnotationModelListener(this);
            // FIXME use some kind of Erlide model-listener
        }
        fListeners.add(listener);
    }

    /**
     * Removes a <code>IProblemChangedListener</code>.
     */
    public void removeListener(final IProblemChangedListener listener) {
        fListeners.remove(listener);
        if (fListeners.isEmpty()) {
            ResourcesPlugin.getWorkspace().removeResourceChangeListener(this);
            // JavaPlugin.getDefault().getCompilationUnitDocumentProvider()
            // .removeGlobalAnnotationModelListener(this);
            // FIXME use some kind of Erlide model-listener
        }
    }

    private void fireChanges(final IResource[] changes,
            final boolean isMarkerChange) {
        final Display display = SWTUtil.getStandardDisplay();
        if (display != null && !display.isDisposed()) {
            display.asyncExec(new Runnable() {
                @Override
                @SuppressWarnings("synthetic-access")
                public void run() {
                    final Object[] listeners = fListeners.getListeners();
                    for (int i = 0; i < listeners.length; i++) {
                        final IProblemChangedListener curr = (IProblemChangedListener) listeners[i];
                        curr.problemsChanged(changes, isMarkerChange);
                    }
                }
            });
        }
    }

}
