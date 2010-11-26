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
package org.erlide.ui.editors.erl.hover;

import java.util.Iterator;

// TODO remove?

/**
 * Interface of annotations representing markers and problems.
 * 
 * @see org.eclipse.core.resources.IMarker
 */
public interface IErlangAnnotation {

    /**
     * @see org.eclipse.jface.text.source.Annotation#getType()
     */
    String getType();

    /**
     * @see org.eclipse.jface.text.source.Annotation#isPersistent()
     */
    boolean isPersistent();

    /**
     * @see org.eclipse.jface.text.source.Annotation#isMarkedDeleted()
     */
    boolean isMarkedDeleted();

    /**
     * @see org.eclipse.jface.text.source.Annotation#getText()
     */
    String getText();

    /**
     * Returns whether this annotation is overlaid.
     * 
     * @return <code>true</code> if overlaid
     */
    boolean hasOverlay();

    /**
     * Returns the overlay of this annotation.
     * 
     * @return the annotation's overlay
     * @since 3.0
     */
    IErlangAnnotation getOverlay();

    /**
     * Returns an iterator for iterating over the annotation which are overlaid
     * by this annotation.
     * 
     * @return an iterator over the overlaid annotations
     */
    Iterator<IErlangAnnotation> getOverlaidIterator();

    /**
     * Adds the given annotation to the list of annotations which are overlaid
     * by this annotations.
     * 
     * @param annotation
     *            the problem annotation
     */
    void addOverlaid(IErlangAnnotation annotation);

    /**
     * Removes the given annotation from the list of annotations which are
     * overlaid by this annotation.
     * 
     * @param annotation
     *            the problem annotation
     */
    void removeOverlaid(IErlangAnnotation annotation);

    /**
     * Tells whether this annotation is a problem annotation.
     * 
     * @return <code>true</code> if it is a problem annotation
     */
    boolean isProblem();

}
