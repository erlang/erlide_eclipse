/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.engine.internal.model.root;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.util.ModelConfig;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.ErlModelStatus;
import org.erlide.engine.model.ErlModelStatusConstants;
import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.util.ErlLogger;

/**
 * Abstract class for implementations of Erlang elements which are IOpenable.
 *
 * @see IErlElement
 * @see IOpenable
 */
public abstract class Openable extends ErlElement implements IOpenable {

    protected IResource findResult;

    protected Openable(final IParent parent, final String name) {
        super(parent, name);
    }

    /**
     * Builds this element's structure and properties in the given info object,
     * based on this element's current contents (reuse buffer contents if this
     * element has an open buffer, or resource contents if this element does not
     * have an open buffer). Children are placed in the given newElements table
     * (note, this element has already been placed in the newElements table).
     * Returns true if successful, or false if an error is encountered while
     * determining the structure of this element.
     *
     * @param dirtyRegion
     */
    public abstract boolean buildStructure(IProgressMonitor pm) throws ErlModelException;

    @Override
    public boolean exists() {
        final IResource resource = getResource();
        if (resource != null) {
            return resource.exists();
        }
        return true;
    }

    @Override
    public synchronized void open(final IProgressMonitor monitor)
            throws ErlModelException {
        if (ModelConfig.verbose) {
            ErlLogger.debug("open " + isStructureKnown() + " > " + this);
        }
        // open the parent if necessary
        openParent(monitor);
        if (monitor != null && monitor.isCanceled()) {
            return;
        }

        // build the structure of the openable (this will open the buffer if
        // needed)
        if (!isStructureKnown()) {
            final boolean knownStructure = buildStructure(monitor);
            setStructureKnown(knownStructure);
        }
    }

    class ErlangResourceVisitor implements IResourceVisitor {

        private final String aname;

        public ErlangResourceVisitor(final String name) {
            aname = name;
        }

        @Override
        public boolean visit(final IResource resource) {
            if (resource.getType() == IResource.FILE && resource.getName().equals(aname)) {
                findResult = resource;
                return false;
            }
            // return true to continue visiting children.
            return true;
        }
    }

    /**
     * Returns true if this element may have an associated source buffer,
     * otherwise false. Subclasses must override as required.
     */
    protected boolean hasBuffer() {
        return false;
    }

    @Override
    public boolean hasUnsavedChanges() throws ErlModelException {

        if (isReadOnly() || !isOpen()) {
            return false;
        }
        // final IBuffer buf = this.getBuffer();
        // if (buf != null && buf.hasUnsavedChanges()) {
        // return true;
        // }
        // for packages and projects must check open buffers
        // to see if they have an child with unsaved changes
        final ErlElementKind elementType = getKind();
        if (elementType == ErlElementKind.PROJECT || elementType == ErlElementKind.MODEL) {
            // final Enumeration openBuffers =
            // getBufferManager().getOpenBuffers();
            // while (openBuffers.hasMoreElements()) {
            // final IBuffer buffer = (IBuffer) openBuffers.nextElement();
            // if (buffer.hasUnsavedChanges()) {
            // final IErlElement owner = (IErlElement) buffer.getOwner();
            // if (isAncestorOf(owner)) {
            // return true;
            // }
            // }
            // }
        }

        return false;
    }

    @Override
    public boolean isConsistent() {
        return true;
    }

    @Override
    public boolean isOpen() {
        return true;
    }

    protected boolean isSourceElement() {
        return false;
    }

    @Override
    public void makeConsistent(final IProgressMonitor monitor) throws ErlModelException {
        if (isConsistent()) {
            return;
        }
    }

    protected void openParent(final IProgressMonitor pm) throws ErlModelException {

        final Openable openableParent = (Openable) getOpenableParent();
        if (openableParent != null && !openableParent.isOpen()) {
            openableParent.open(pm);
        }
    }

    protected boolean parentExists() {
        final IParent parent = getParent();
        if (parent == null) {
            return true;
        }
        if (parent instanceof IErlElement) {
            final IErlElement element = (IErlElement) parent;
            return element.exists();
        }
        return false;
    }

    protected boolean resourceExists() {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        if (workspace == null) {
            return false;
        }
        return ErlangEngine
                .getInstance()
                .getModelUtilService()
                .getTarget(workspace.getRoot(),
                        getResource().getFullPath().makeRelative(), true) != null;
    }

    @Override
    public void save(final IProgressMonitor pm, final boolean force)
            throws ErlModelException {
        if (isReadOnly()) {
            throw new ErlModelException(new ErlModelStatus(
                    ErlModelStatusConstants.READ_ONLY, this));
        }
        // final IBuffer buf = getBuffer();
        // if (buf != null) { // some Openables (like a ErlProject) don't have a
        // // buffer
        // buf.save(pm, force);
        // makeConsistent(pm); // update the element info of this
        // // element
        // }
    }

    @Override
    public void close() throws ErlModelException {
        for (final IErlElement child : getChildren()) {
            if (child instanceof IOpenable) {
                final IOpenable openable = (IOpenable) child;
                if (openable.isOpen()) {
                    openable.close();
                }
            }
        }
        internalGetChildren().clear();
        setStructureKnown(false);
    }
}
