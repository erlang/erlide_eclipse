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
package org.erlide.engine.model;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;

/**
 * Common protocol for Erlang elements that contain other Erlang elements.
 *
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface IParent {

    /**
     * Returns the immediate children of this element. Unless otherwise
     * specified by the implementing element, the children are in no particular
     * order.
     *
     * @exception ErlModelException
     *                if this element does not exist or if an exception occurs
     *                while accessing its corresponding resource
     * @return the immediate children of this element
     */
    List<IErlElement> getChildren() throws ErlModelException;

    int getChildCount();

    /**
     * Returns whether this element has one or more immediate children. This is
     * a convenience method, and may be more efficient than testing whether
     * <code>getChildren</code> is an empty array.
     *
     * @exception ErlModelException
     *                if this element does not exist or if an exception occurs
     *                while accessing its corresponding resource
     * @return true if the immediate children of this element, false otherwise
     */
    boolean hasChildren();

    List<IErlElement> getChildrenOfKind(ErlElementKind... kind) throws ErlModelException;

    boolean hasChildrenOfKind(ErlElementKind... kind);

    IErlElement getChildNamed(String s);

    IErlElement getChildWithResource(IResource rsrc);

    void addChild(IErlElement child);

    public void setChildren(final Collection<? extends IErlElement> children);

    void removeChild(IErlElement e);

}
