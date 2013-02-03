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
package org.erlide.core.model.root;

import org.eclipse.core.resources.IResourceDelta;

/**
 * A Erlang element delta describes changes in Erlang element between two
 * discrete points in time. Given a delta, clients can access the element that
 * has changed, and any children that have changed.
 * <p>
 * Deltas have a different status depending on the kind of change they
 * represent. The list below summarizes each status (as returned by
 * <code>getKind</code>) and its meaning (see individual constants for a more
 * detailled description):
 * <ul>
 * <li><code>ADDED</code>- The element described by the delta has been added.</li>
 * <li><code>REMOVED</code>- The element described by the delta has been
 * removed.</li>
 * <li><code>CHANGED</code>- The element described by the delta has been changed
 * in some way. Specification of the type of change is provided by
 * <code>getFlags</code> which returns the following values:
 * <ul>
 * <li><code>F_ADDED_TO_CLASSPATH</code>- A classpath entry corresponding to the
 * element has been added to the project's classpath. This flag is only valid if
 * the element is an <code>IPackageFragmentRoot</code>.</li>
 * <li><code>F_CHILDREN</code>- A child of the element has changed in some way.
 * This flag is only valid if the element is an <code>IParent</code>.</li>
 * <li><code>F_CLASSPATH_REORDER</code>- A classpath entry corresponding to the
 * element has changed position in the project's classpath. This flag is only
 * valid if the element is an <code>IPackageFragmentRoot</code>.</li>
 * <li><code>F_CLOSED</code>- The underlying <code>IProject</code> has been
 * closed. This flag is only valid if the element is an <code>IErlProject</code>
 * .</li>
 * <li><code>F_CONTENT</code>- The contents of the element have been altered.
 * This flag is only valid for elements which correspond to files.</li>
 * <li><code>F_FINE_GRAINED</code>- The delta is a fine-grained delta, that is,
 * an analysis down to the members level was done to determine if there were
 * structural changes to members of the element.</li>
 * <li><code>F_OPENED</code>- The underlying <code>IProject</code> has been
 * opened. This flag is only valid if the element is an <code>IErlProject</code>
 * .</li>
 * <li><code>F_REMOVED_FROM_CLASSPATH</code>- A classpath entry corresponding to
 * the element has been removed from the project's classpath. This flag is only
 * valid if the element is an <code>IPackageFragmentRoot</code>.</li>
 * </ul>
 * </li>
 * </ul>
 * </p>
 * <p>
 * Move operations are indicated by other change flags, layered on top of the
 * change flags described above. If element A is moved to become B, the delta
 * for the change in A will have status <code>REMOVED</code>, with change flag
 * <code>F_MOVED_TO</code>. In this case, <code>getMovedToElement</code> on
 * delta A will return the handle for B. The delta for B will have status
 * <code>ADDED</code>, with change flag <code>F_MOVED_FROM</code>, and
 * <code>getMovedFromElement</code> on delta B will return the handle for A.
 * (Note, the handle to A in this case represents an element that no longer
 * exists).
 * </p>
 * <p>
 * Note that the move change flags only describe the changes to a single
 * element, they do not imply anything about the parent or children of the
 * element.
 * </p>
 * <p>
 * The <code>F_ADDED_TO_CLASSPATH</code>,<code>F_REMOVED_FROM_CLASSPATH</code>
 * and <code>F_CLASSPATH_REORDER</code> flags are triggered by changes to a
 * project's classpath. They do not mean that the underlying resource was added,
 * removed or changed. For example, if a project P already contains a folder
 * src, then adding a classpath entry with the 'P/src' path to the project's
 * classpath will result in an <code>IErlElementDelta</code> with the
 * <code>F_ADDED_TO_CLASSPATH</code> flag for the
 * <code>IPackageFragmentRoot</code> P/src. On the contrary, if a resource is
 * physically added, removed or changed and this resource corresponds to a
 * classpath entry of the project, then an <code>IErlElementDelta</code> with
 * the <code>ADDED</code>, <code>REMOVED</code>, or <code>CHANGED</code> kind
 * will be fired.
 * </p>
 * <p>
 * No assumptions should be made on whether the erlang element delta tree is
 * rooted at the <code>IErlModel</code> level or not.
 * </p>
 * <p>
 * <code>IErlElementDelta</code> object are not valid outside the dynamic scope
 * of the notification.
 * </p>
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 */
public interface IErlElementDelta {

    /**
     * Status constant indicating that the element has been added. Note that an
     * added java element delta has no children, as they are all implicitely
     * added.
     */
    int ADDED = 1;

    /**
     * Status constant indicating that the element has been removed. Note that a
     * removed java element delta has no children, as they are all implicitely
     * removed.
     */
    int REMOVED = 2;

    /**
     * Status constant indicating that the element has been changed, as
     * described by the change flags.
     * 
     * @see #getFlags()
     */
    int CHANGED = 4;

    /**
     * Status constant used for getChildren
     */
    int ALL = 0;

    /**
     * Change flag indicating that the content of the element has changed. This
     * flag is only valid for elements which correspond to files.
     */
    int F_CONTENT = 0x00001;

    /**
     * Change flag indicating that there are changes to the children of the
     * element. This flag is only valid if the element is an
     * <code>IParent</code>.
     */
    int F_CHILDREN = 0x00008;

    /**
     * Change flag indicating that the element was moved from another location.
     * The location of the old element can be retrieved using
     * <code>getMovedFromElement</code>.
     */
    int F_MOVED_FROM = 0x00010;

    /**
     * Change flag indicating that the element was moved to another location.
     * The location of the new element can be retrieved using
     * <code>getMovedToElement</code>.
     */
    int F_MOVED_TO = 0x00020;

    /**
     * Change flag indicating that a classpath entry corresponding to the
     * element has been added to the project's classpath. This flag is only
     * valid if the element is an <code>IPackageFragmentRoot</code>.
     */
    int F_ADDED_TO_CLASSPATH = 0x00040;

    /**
     * Change flag indicating that a classpath entry corresponding to the
     * element has been removed from the project's classpath. This flag is only
     * valid if the element is an <code>IPackageFragmentRoot</code>.
     */
    int F_REMOVED_FROM_CLASSPATH = 0x00080;

    /**
     * Change flag indicating that the element has changed position relatively
     * to its siblings. If the element is an <code>IPackageFragmentRoot</code>,
     * a classpath entry corresponding to the element has changed position in
     * the project's classpath.
     * 
     * 
     */
    int F_REORDER = 0x00100;

    /**
     * Change flag indicating that the underlying <code>IProject</code> has been
     * opened. This flag is only valid if the element is an
     * <code>IErlProject</code>.
     */
    int F_OPENED = 0x00200;

    /**
     * Change flag indicating that the underlying <code>IProject</code> has been
     * closed. This flag is only valid if the element is an
     * <code>IErlProject</code>.
     */
    int F_CLOSED = 0x00400;

    /**
     * Change flag indicating that this is a fine-grained delta, that is, an
     * analysis down to the members level was done to determine if there were
     * structural changes to members.
     * <p>
     * Clients can use this flag to find out if a compilation unit that have a
     * <code>F_CONTENT</code> change should assume that there are no finer
     * grained changes (<code>F_FINE_GRAINED</code> is set) or if finer grained
     * changes were not considered (<code>F_FINE_GRAINED</code> is not set).
     * 
     * 
     */
    int F_FINE_GRAINED = 0x04000;

    /**
     * Change flag indicating that the raw classpath (or the output folder) of a
     * project has changed. This flag is only valid if the element is an
     * <code>IErlProject</code>.
     * 
     * 
     */
    int F_CLASSPATH_CHANGED = 0x20000;

    /**
     * A source entry added for this resource.
     */
    public int F_ADDED_PATHENTRY_SOURCE = 0x0100;

    /**
     * A source entry was remove for this resource.
     */
    public int F_REMOVED_PATHENTRY_SOURCE = 0x0200;

    /**
     * A pathEntry Macro was added for this resource
     */
    public int F_CHANGED_PATHENTRY_MACRO = 0x0400;

    /**
     * A pathEntry Include was added for this resource
     */
    public int F_CHANGED_PATHENTRY_INCLUDE = 0x0800;

    /**
     * A pathEntry Library was added for this resource
     */
    public int F_ADDED_PATHENTRY_LIBRARY = 0x01000;

    /**
     * A pathEntry Library was added for this resource
     */
    public int F_REMOVED_PATHENTRY_LIBRARY = 0x02000;

    /**
     * A pathEntry Project was added to the project.
     */
    public int F_CHANGED_PATHENTRY_PROJECT = 0x04000;

    /**
     * Reordering of the path entries.
     */
    public int F_PATHENTRY_REORDER = 0x040000;

    // public int F_SUPER_TYPES = 0x080000;

    /**
     * Change flag indicating that a source jar has been attached to a binary
     * jar.
     */
    public int F_SOURCEATTACHED = 0x100000;

    /**
     * Change flag indicating that a source jar has been detached to a binary
     * jar.
     */
    public int F_SOURCEDETACHED = 0x200000;

    /**
     * Returns deltas for the children that have been added, removed or changed
     * as specified.
     * 
     * @param kind
     *            kind of children
     * 
     * @return deltas for the children that have been added
     */
    IErlElementDelta[] getChildren(int kind);

    /**
     * Returns the element that this delta describes a change to.
     * 
     * @return the element that this delta describes a change to
     */
    IErlElement getElement();

    /**
     * Returns flags that describe how an element has changed. Such flags should
     * be tested using the <code>&</code> operand. For example:
     * 
     * <pre>
     * if ((delta.getFlags() &amp; IErlElementDelta.F_CONTENT) != 0) {
     *     // the delta indicates a content change
     * }
     * </pre>
     * 
     * @return flags that describe how an element has changed
     */
    int getFlags();

    /**
     * Returns the kind of this delta - one of <code>ADDED</code>,
     * <code>REMOVED</code>, or <code>CHANGED</code>.
     * 
     * @return the kind of this delta
     */
    int getKind();

    /**
     * Returns an element describing this element before it was moved to its
     * current location, or <code>null</code> if the <code>F_MOVED_FROM</code>
     * change flag is not set.
     * 
     * @return an element describing this element before it was moved to its
     *         current location, or <code>null</code> if the
     *         <code>F_MOVED_FROM</code> change flag is not set
     */
    IErlElement getMovedFromElement();

    /**
     * Returns an element describing this element in its new location, or
     * <code>null</code> if the <code>F_MOVED_TO</code> change flag is not set.
     * 
     * @return an element describing this element in its new location, or
     *         <code>null</code> if the <code>F_MOVED_TO</code> change flag is
     *         not set
     */
    IErlElement getMovedToElement();

    /**
     * Returns the collection of resource deltas.
     * <p>
     * Note that resource deltas, like Erlang element deltas, are generally only
     * valid for the dynamic scope of an event notification. Clients must not
     * hang on to these objects.
     * </p>
     * 
     * @return the underlying resource deltas, or <code>null</code> if none
     */
    IResourceDelta[] getResourceDeltas();

    /**
     * Find delta with specified element (either this or among children)
     * 
     * @param element
     *            the element to find
     * @return the delta for the specified element
     */
    IErlElementDelta findElement(IErlElement element);
}
