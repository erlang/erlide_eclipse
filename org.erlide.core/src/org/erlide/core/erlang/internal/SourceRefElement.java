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
package org.erlide.core.erlang.internal;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IOpenable;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.ISourceManipulation;
import org.erlide.core.erlang.ISourceRange;
import org.erlide.core.erlang.ISourceReference;
import org.erlide.core.erlang.SourceRange;
import org.erlide.jinterface.backend.util.Util;

/**
 * Abstract class for Erlang elements which implement ISourceReference.
 */
abstract class SourceRefElement extends ErlElement implements ISourceReference {

    protected int fSourceRangeOffset;
    protected int fSourceRangeLength;
    protected int lineStart, lineEnd;

    protected SourceRefElement(final IParent parent, final String name) {
        super(parent, name);
    }

    /**
     * This element is being closed. Do any necessary cleanup.
     */
    @Override
    protected void closing(final Object info) throws ErlModelException {
        // Do any necessary cleanup
    }

    /**
     * Returns a new element info for this element.
     */
    protected Object createElementInfo() {
        return null; // not used for source ref elements
    }

    /**
     * @see ISourceManipulation
     */
    public void copy(final IErlElement container, final IErlElement sibling,
            final String rename, final boolean force,
            final IProgressMonitor monitor) throws ErlModelException {
        if (container == null) {
            throw new IllegalArgumentException(
                    Util.bind("operation.nullContainer")); //$NON-NLS-1$
        }
        final IErlElement[] elements = new IErlElement[] { this };
        final IErlElement[] containers = new IErlElement[] { container };
        IErlElement[] siblings = null;
        if (sibling != null) {
            siblings = new IErlElement[] { sibling };
        }
        String[] renamings = null;
        if (rename != null) {
            renamings = new String[] { rename };
        }
        getModel().copy(elements, containers, siblings, renamings, force,
                monitor);
    }

    /**
     * @see ISourceManipulation
     */
    public void delete(final boolean force, final IProgressMonitor monitor)
            throws ErlModelException {
        final IErlElement[] elements = new IErlElement[] { this };
        getModel().delete(elements, force, monitor);
    }

    /*
     * @see ErlElement#generateInfos
     */
    protected void open(final IProgressMonitor pm) throws ErlModelException {
        final Openable openableParent = (Openable) getOpenableParent();
        if (openableParent == null) {
            return;
        }

        final ErlElement openableParentInfo = (ErlElement) ErlangCore
                .getModelManager().getInfo(openableParent);
        if (openableParentInfo == null) {
            openableParent.open(pm);
        }
    }

    /**
     * Elements within compilation units and class files have no corresponding
     * resource.
     * 
     * @see IErlElement
     */
    @Override
    public IResource getCorrespondingResource() throws ErlModelException {
        if (!exists()) {
            throw newNotPresentException();
        }
        return null;
    }

    /**
     * Return the first instance of IOpenable in the hierarchy of this type
     * (going up the hierarchy from this type);
     */
    @Override
    public IOpenable getOpenableParent() {
        IParent parent = getParent();
        while (parent != null) {
            if (parent instanceof IOpenable) {
                return (IOpenable) parent;
            }
            if (parent instanceof IErlElement) {
                final IErlElement parentElement = (IErlElement) parent;
                parent = parentElement.getParent();
            } else {
                break;
            }
        }
        return null;
    }

    /**
     * @see ISourceReference
     */
    // public String getSource() throws ErlModelException {
    // // final IOpenable openable = getOpenableParent();
    // // final IBuffer buffer = openable.getBuffer();
    // // if (buffer == null) {
    // // return null;
    // // }
    // // final ISourceRange range = getSourceRange();
    // // final int offset = range.getOffset();
    // // final int length = range.getLength();
    // // if (offset == -1 || length == 0) {
    // // return null;
    // // }
    // // try {
    // // return buffer.getText(offset, length);
    // // } catch (final RuntimeException e) {
    // // return null;
    // // }
    // return "";
    // }
    /**
     * @see ISourceReference
     */
    public ISourceRange getSourceRange() throws ErlModelException {
        return new SourceRange(fSourceRangeOffset, fSourceRangeLength);
    }

    // /**
    // * @see IErlElement
    // */
    // public IResource getUnderlyingResource() throws ErlModelException {
    // if (!exists()) {
    // throw newNotPresentException();
    // }
    // return getParent().getUnderlyingResource();
    // }

    /**
     * @see IParent
     */
    @Override
    public boolean hasChildren() {
        try {
            return getChildren().size() > 0;
        } catch (final ErlModelException e) {
        }
        return false;
    }

    /**
     * @see ISourceManipulation
     */
    public void move(final IErlElement container, final IErlElement sibling,
            final String rename, final boolean force,
            final IProgressMonitor monitor) throws ErlModelException {
        if (container == null) {
            throw new IllegalArgumentException(
                    Util.bind("operation.nullContainer")); //$NON-NLS-1$
        }
        final IErlElement[] elements = new IErlElement[] { this };
        final IErlElement[] containers = new IErlElement[] { container };
        IErlElement[] siblings = null;
        if (sibling != null) {
            siblings = new IErlElement[] { sibling };
        }
        String[] renamings = null;
        if (rename != null) {
            renamings = new String[] { rename };
        }
        getModel().move(elements, containers, siblings, renamings, force,
                monitor);
    }

    /**
     * @see ISourceManipulation
     */
    public void rename(final String newName, final boolean force,
            final IProgressMonitor monitor) throws ErlModelException {
        if (newName == null) {
            throw new IllegalArgumentException(Util.bind("element.nullName")); //$NON-NLS-1$
        }
        final IErlElement[] elements = new IErlElement[] { this };
        final IErlElement[] dests = new IErlElement[] { (IErlElement) getParent() };
        final String[] renamings = new String[] { newName };
        getModel().rename(elements, dests, renamings, force, monitor);
    }

    public void setSourceRangeOffset(final int offset) {
        fSourceRangeOffset = offset;
    }

    public void setSourceRangeLength(final int length) {
        fSourceRangeLength = length;
    }

    protected void setLineStart(final int lineStart) {
        this.lineStart = lineStart;
    }

    public int getLineStart() {
        return lineStart;
    }

    protected void setLineEnd(final int lineEnd) {
        this.lineEnd = lineEnd;
    }

    public int getLineEnd() {
        return lineEnd;
    }

    @Override
    public boolean equals(final Object o) {
        if (!super.equals(o) || !(o instanceof SourceRefElement)) {
            return false;
        }
        final SourceRefElement r = (SourceRefElement) o;
        return fSourceRangeOffset == r.fSourceRangeOffset
                && fSourceRangeLength == r.fSourceRangeLength;
    }

}
