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
package org.erlide.core.internal.model.erlang;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.internal.model.root.ErlElement;
import org.erlide.core.internal.model.root.Openable;
import org.erlide.core.model.erlang.ISourceRange;
import org.erlide.core.model.erlang.ISourceReference;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IOpenable;
import org.erlide.core.model.root.IParent;

import com.google.common.base.Objects;

/**
 * Abstract class for Erlang elements which implement ISourceReference.
 */
public abstract class SourceRefElement extends ErlElement implements
        ISourceReference {

    protected int fSourceRangeOffset;
    protected int fSourceRangeLength;
    protected int lineStart, lineEnd;

    protected SourceRefElement(final IParent parent, final String name) {
        super(parent, name);
    }

    /**
     * Returns a new element info for this element.
     */
    protected Object createElementInfo() {
        return null; // not used for source ref elements
    }

    /*
     * @see ErlElement#generateInfos
     */
    protected void open(final IProgressMonitor pm) throws ErlModelException {
        final Openable openableParent = (Openable) getOpenableParent();
        if (openableParent == null) {
            return;
        }

        openableParent.open(pm);
    }

    /**
     * Elements within compilation units and class files have no corresponding
     * resource.
     * 
     * @see IErlElement
     */
    @Override
    public IResource getCorrespondingResource() {
        // if (!exists()) {
        // throw newNotPresentException();
        // }
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
    @Override
    public ISourceRange getSourceRange() {
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
        synchronized (getModelLock()) {
            return internalGetChildren().size() > 0;
        }
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

    @Override
    public int getLineStart() {
        return lineStart;
    }

    protected void setLineEnd(final int lineEnd) {
        this.lineEnd = lineEnd;
    }

    @Override
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

    @Override
    public int hashCode() {
        return Objects.hashCode(super.hashCode(), fSourceRangeOffset,
                fSourceRangeLength);
    }

    @Override
    public String getSource() throws ErlModelException {
        throw new UnsupportedOperationException();
    }
}
