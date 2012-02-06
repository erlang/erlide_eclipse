/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.model.root;

import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.utils.IDisposable;

/**
 * Common protocol for all elements provided by the Erlang model. Erlang model
 * elements are exposed to clients as handles to the actual underlying element.
 * The Erlang model may hand out any number of handles for each element. Handles
 * that refer to the same element are guaranteed to be equal, but not
 * necessarily identical.
 * <p>
 * Methods annotated as "handle-only" do not require underlying elements to
 * exist. Methods that require underlying elements to exist throw a
 * <code>ErlModelException</code> when an underlying element is missing.
 * <code>ErlModelException.isDoesNotExist</code> can be used to recognize this
 * common special case.
 * </p>
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 * @author jakob
 */
public interface IErlElement extends IAdaptable, IDisposable {

    enum Kind {
        //@formatter:off
        ERROR, 
        MODEL, 
        PROJECT, 
        MODULE, 
        ATTRIBUTE, 
        FUNCTION, 
        CLAUSE, 
        EXPORT, 
        IMPORT, 
        EXPORTFUNCTION, 
        HEADERCOMMENT, 
        COMMENT, 
        RECORD_DEF, 
        MACRO_DEF, 
        FOLDER, 
        TYPESPEC, 
        EXTERNAL, 
        RECORD_FIELD 
        //@formatter:on
    }

    /**
     * Returns whether this Erlang element exists in the model.
     * <p>
     * Erlang elements are handle objects that may or may not be backed by an
     * actual element. Erlang elements that are backed by an actual element are
     * said to "exist", and this method returns <code>true</code>. It is always
     * the case that if the element exists, then its parent also exists
     * (provided it has one) and includes the element as one of its children. It
     * is therefore possible to navigate to any existing Erlang element from the
     * root of the Erlang model along a chain of existing Erlang elements.
     * </p>
     * 
     * @return <code>true</code> if this element exists in the Erlang model, and
     *         <code>false</code> if this element does not exist
     */
    boolean exists();

    /**
     * Returns the first ancestor of this Erlang element that has the given
     * type. Returns <code>null</code> if no such an ancestor can be found. This
     * is a handle-only method.
     * 
     * @param kind
     *            the given kind
     * @return the first ancestor of this Erlang element that has the given
     *         kind, null if no such an ancestor can be found
     */
    IErlElement getAncestorOfKind(Kind kind);

    /**
     * Returns the enclosing IErlProject if there is one
     */
    IErlProject getProject();

    /**
     * Returns the resource that corresponds directly to this element, or
     * <code>null</code> if there is no resource that corresponds to this
     * element.
     * <p>
     * For example, the corresponding resource for an <code>IErlModule</code> is
     * its underlying <code>IFile</code>. There are no corresponding resources
     * for <code>IAttributes</code>,<code>IFunctions</code>, etc.
     * <p>
     * This differs from getResource, which returns the parents resource, if the
     * element doesn't correspond directly to a resource.
     * 
     * @return the corresponding resource, or <code>null</code> if none
     */
    IResource getCorrespondingResource();

    /**
     * Returns the name of this element. This is a handle-only method.
     * 
     * @return the element name
     */
    String getName();

    /**
     * Returns this element's kind encoded as an integer. This is a handle-only
     * method.
     * 
     * @return the kind of element; one of the constants declared in
     *         <code>IErlElement</code>
     * @see IErlElement
     */
    Kind getKind();

    /**
     * Returns the Erlang model. This is a handle-only method.
     * 
     * @return the Erlang model
     */
    IErlModel getModel();

    /**
     * Returns the element directly containing this element, or
     * <code>null</code> if this element has no parent. This is a handle-only
     * method.
     * 
     * @return the parent element, or <code>null</code> if this element has no
     *         parent
     */
    IParent getParent();

    /**
     * Returns the innermost resource enclosing this element. This is a
     * handle-only method.
     * 
     * @return the innermost resource enclosing this element, or
     *         <code>null</code>
     * 
     */
    IResource getResource();

    /**
     * Returns the scheduling rule associated with this Erlang element. This is
     * a handle-only method.
     * 
     * @return the scheduling rule associated with this Erlang element
     * 
     */
    ISchedulingRule getSchedulingRule();

    // /**
    // * Returns the smallest underlying resource that contains this element, or
    // * <code>null</code> if this element is not contained in a resource.
    // *
    // * @return the underlying resource, or <code>null</code> if none
    // * @throws ErlModelException
    // * if this element does not exist or if an exception occurs
    // * while accessing its underlying resource
    // */
    // IResource getUnderlyingResource() throws ErlModelException;

    /**
     * Returns whether this Erlang element is read-only. An element is read-only
     * if its structure cannot be modified by the Erlang model.
     * <p>
     * Note this is different from IResource.isReadOnly().
     * <p>
     * This is a handle-only method.
     * 
     * @return <code>true</code> if this element is read-only
     */
    boolean isReadOnly();

    /**
     * Returns whether the structure of this element is known. For example, for
     * a compilation unit that could not be parsed, <code>false</code> is
     * returned. If the structure of an element is unknown, navigations will
     * return reasonable defaults. For example, <code>getChildren</code> will
     * return an empty collection.
     * <p>
     * Note: This does not imply anything about consistency with the underlying
     * resource/buffer contents.
     * </p>
     * 
     * @return <code>true</code> if the structure of this element is known
     * @throws ErlModelException
     *             if this element does not exist or if an exception occurs
     *             while accessing its corresponding resource
     */
    boolean isStructureKnown() throws ErlModelException;

    void resourceChanged(IResourceDelta delta);

    // static final int VISIT_REFERENCED = 0x0001;
    // static final int VISIT_EXTERNALS = 0x0002;
    enum AcceptFlags {
        LEAFS_ONLY, CHILDREN_FIRST
    }

    /**
     * The good old visitor pattern
     * 
     * @param visitor
     * @param flags
     * @param leafKind
     * @throws ErlModelException
     */
    void accept(IErlElementVisitor visitor, Set<AcceptFlags> flags,
            IErlElement.Kind leafKind) throws ErlModelException;

    // TODO StyledString? but this needs jface-stuff, which we don't want in a
    // non-ui class, probably we should get rid of this one and put everything
    // in the label provider...
    String getLabelString();

    /**
     * Return the file path of the underlying element, i.e. if it's a module,
     * the file path to the element being edited
     * 
     * @return path or null if not a file-based element
     */
    String getFilePath();

    void clearCaches();

    String toStringWithAncestors();

    Object getModelLock();

    IErlModule getModule();
}
