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
package org.erlide.core.model.util;

import java.util.EventObject;

import org.erlide.core.model.root.IErlElementDelta;

/**
 * An element changed event describes a change to the structure or contents of a
 * tree of Erlang elements. The changes to the elements are described by the
 * associated delta object carried by this event.
 * <p>
 * This class is not intended to be instantiated or subclassed by clients.
 * Instances of this class are automatically created by the Erlang model.
 * </p>
 * 
 * @see IElementChangedListener
 * @see IErlElementDelta
 */
public class ElementChangedEvent extends EventObject {

    private static final long serialVersionUID = 1L;

    /**
     * Event type constant (bit mask) indicating an after-the-fact report of
     * creations, deletions, and modifications to one or more Erlang element(s)
     * expressed as a hierarchical Erlang element delta as returned by
     * <code>getDelta()</code>.
     * 
     * Note: this notification occurs during the corresponding POST_CHANGE
     * resource change notification, and contains a full delta accounting for
     * any ErlangModel operation and/or resource change.
     * 
     * @see IErlElementDelta
     * @see org.eclipse.core.resources.IResourceChangeEvent
     * @see #getDelta()
     */
    public static final int POST_CHANGE = 1;

    /**
     * Event type constant (bit mask) indicating an after-the-fact report of
     * creations, deletions, and modifications to one or more Erlang element(s)
     * expressed as a hierarchical Erlang element delta as returned by
     * <code>getDelta</code>.
     * 
     * Note: this notification occurs as a result of a working copy reconcile
     * operation.
     * 
     * @see IErlElementDelta
     * @see org.eclipse.core.resources.IResourceChangeEvent
     * @see #getDelta()
     */
    public static final int POST_RECONCILE = 4;

    /**
     * Event type constant indicating the following: Source text is changed
     * somewhere in function body No global data affected for any erl element
     * but element offsets should be recalculated now.
     * 
     * Note: usually, ErlShiftData object is sent with this event as
     * IErlElementDelta
     * 
     * @see CShiftData
     */
    public static final int POST_SHIFT = 5;

    /*
     * Event type indicating the nature of this event. It can be a combination
     * either: - POST_CHANGE - PRE_AUTO_BUILD - POST_RECONCILE
     */
    private final int fType;

    /**
     * Creates an new element changed event (based on a
     * <code>IErlElementDelta</code>).
     * 
     * @param delta
     *            the Erlang element delta.
     * @param type
     *            the type of delta (ADDED, REMOVED, CHANGED) this event
     *            contains
     */
    public ElementChangedEvent(final IErlElementDelta delta, final int type) {
        super(delta);
        fType = type;
    }

    /**
     * Returns the delta describing the change.
     * 
     * @return the delta describing the change
     */
    public IErlElementDelta getDelta() {
        return (IErlElementDelta) source;
    }

    /**
     * Returns the type of event being reported.
     * 
     * @return one of the event type constants
     * @see #POST_CHANGE
     * @see #PRE_AUTO_BUILD
     * @see #POST_RECONCILE
     */
    public int getType() {
        return fType;
    }
}
