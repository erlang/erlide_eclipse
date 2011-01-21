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
package org.erlide.core.erlang;

import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.IProgressMonitor;

import com.ericsson.otp.erlang.OtpErlangList;

import erlang.FunctionRef;

/**
 * Represent the root Erlang element corresponding to the workspace. Since there
 * is only one such root element, it is commonly referred to as <em>the</em>
 * Erlang model element. The Erlang model element needs to be opened before it
 * can be navigated or manipulated. The Erlang model element has no parent (it
 * is the root of the Erlang element hierarchy). Its children are
 * <code>IErlProject</code>s.
 * <p>
 * This interface provides methods for performing copy, move, rename, and delete
 * operations on multiple Erlang elements.
 * </p>
 * <p>
 * This interface is not intended to be implemented by clients. An instance of
 * one of these handles can be created via
 * <code>ErlangCore.create(workspace.getRoot())</code>.
 * </p>
 * 
 * @see ErlangCore#create(org.eclipse.core.resources.IWorkspaceRoot)
 */
public interface IErlModel extends IErlElement, IOpenable, IParent {

    /**
     * Copies the given elements to the specified container(s). If one container
     * is specified, all elements are copied to that container. If more than one
     * container is specified, the number of elements and containers must match,
     * and each element is copied to its associated container.
     * <p>
     * Optionally, each copy can positioned before a sibling element. If
     * <code>null</code> is specified for a given sibling, the copy is inserted
     * as the last child of its associated container.
     * </p>
     * <p>
     * Optionally, each copy can be renamed. If <code>null</code> is specified
     * for the new name, the copy is not renamed.
     * </p>
     * <p>
     * Optionally, any existing child in the destination container with the same
     * name can be replaced by specifying <code>true</code> for force. Otherwise
     * an exception is thrown in the event that a name collision occurs.
     * </p>
     * 
     * @param elements
     *            the elements to copy
     * @param containers
     *            the container, or list of containers
     * @param siblings
     *            the list of siblings element any of which may be
     *            <code>null</code>; or <code>null</code>
     * @param renamings
     *            the list of new names any of which may be <code>null</code>;
     *            or <code>null</code>
     * @param replace
     *            <code>true</code> if any existing child in a target container
     *            with the target name should be replaced, and
     *            <code>false</code> to throw an exception in the event of a
     *            name collision
     * @param monitor
     *            a progress monitor
     * @throws ErlModelException
     *             if an element could not be copied. Reasons include:
     *             <ul>
     *             <li>There is no element to process (NO_ELEMENTS_TO_PROCESS).
     *             The given elements is null or empty</li>
     *             <li>A specified element, container, or sibling does not exist
     *             (ELEMENT_DOES_NOT_EXIST)</li>
     *             <li>A <code>CoreException</code> occurred while updating an
     *             underlying resource</li>
     *             <li>A container is of an incompatible type (
     *             <code>INVALID_DESTINATION</code>)</li>
     *             <li>A sibling is not a child of it associated container (
     *             <code>INVALID_SIBLING</code>)</li>
     *             <li>A new name is invalid (<code>INVALID_NAME</code>)</li>
     *             <li>A child in its associated container already exists with
     *             the same name and <code>replace</code> has been specified as
     *             <code>false</code>(<code>NAME_COLLISION</code>)</li>
     *             <li>A container or element is read-only (
     *             <code>READ_ONLY</code>)</li>
     *             </ul>
     */
    void copy(IErlElement[] elements, IErlElement[] containers,
            IErlElement[] siblings, String[] renamings, boolean replace,
            IProgressMonitor monitor) throws ErlModelException;

    /**
     * Deletes the given elements, forcing the operation if necessary and
     * specified.
     * 
     * @param elements
     *            the elements to delete
     * @param force
     *            a flag controlling whether underlying resources that are not
     *            in sync with the local file system will be tolerated
     * @param monitor
     *            a progress monitor
     * @throws ErlModelException
     *             if an element could not be deleted. Reasons include:
     *             <ul>
     *             <li>There is no element to process (NO_ELEMENTS_TO_PROCESS).
     *             The given elements is null or empty</li>
     *             <li>A specified element does not exist
     *             (ELEMENT_DOES_NOT_EXIST)</li>
     *             <li>A <code>CoreException</code> occurred while updating an
     *             underlying resource</li>
     *             <li>An element is read-only (<code>READ_ONLY</code>)</li>
     *             </ul>
     */
    void delete(IErlElement[] elements, boolean force, IProgressMonitor monitor)
            throws ErlModelException;

    /**
     * Returns the Erlang project with the given name. This is a handle-only
     * method. The project may or may not exist.
     * 
     * @param name
     *            the name of the Erlang project
     * @return the Erlang project with the given name
     */
    IErlProject getErlangProject(String name);

    /**
     * Returns the Erlang projects in this Erlang model, or an empty array if
     * there are none.
     * 
     * @return the Erlang projects in this Erlang model, or an empty array if
     *         there are none
     * @throws ErlModelException
     *             if this request fails.
     */
    Collection<IErlProject> getErlangProjects() throws ErlModelException;

    /**
     * Returns the workspace associated with this Erlang model.
     * 
     * @return the workspace associated with this Erlang model
     */
    IWorkspace getWorkspace();

    /**
     * Moves the given elements to the specified container(s). If one container
     * is specified, all elements are moved to that container. If more than one
     * container is specified, the number of elements and containers must match,
     * and each element is moved to its associated container.
     * <p>
     * Optionally, each element can positioned before a sibling element. If
     * <code>null</code> is specified for sibling, the element is inserted as
     * the last child of its associated container.
     * </p>
     * <p>
     * Optionally, each element can be renamed. If <code>null</code> is
     * specified for the new name, the element is not renamed.
     * </p>
     * <p>
     * Optionally, any existing child in the destination container with the same
     * name can be replaced by specifying <code>true</code> for force. Otherwise
     * an exception is thrown in the event that a name collision occurs.
     * </p>
     * 
     * @param elements
     *            the elements to move
     * @param containers
     *            the container, or list of containers
     * @param siblings
     *            the list of siblings element any of which may be
     *            <code>null</code>; or <code>null</code>
     * @param renamings
     *            the list of new names any of which may be <code>null</code>;
     *            or <code>null</code>
     * @param replace
     *            <code>true</code> if any existing child in a target container
     *            with the target name should be replaced, and
     *            <code>false</code> to throw an exception in the event of a
     *            name collision
     * @param monitor
     *            a progress monitor
     * @throws ErlModelException
     *             if an element could not be moved. Reasons include:
     *             <ul>
     *             <li>There is no element to process (NO_ELEMENTS_TO_PROCESS).
     *             The given elements is null or empty</li>
     *             <li>A specified element, container, or sibling does not exist
     *             (ELEMENT_DOES_NOT_EXIST)</li>
     *             <li>A <code>CoreException</code> occurred while updating an
     *             underlying resource</li>
     *             <li>A container is of an incompatible type (
     *             <code>INVALID_DESTINATION</code>)</li>
     *             <li>A sibling is not a child of it associated container (
     *             <code>INVALID_SIBLING</code>)</li>
     *             <li>A new name is invalid (<code>INVALID_NAME</code>)</li>
     *             <li>A child in its associated container already exists with
     *             the same name and <code>replace</code> has been specified as
     *             <code>false</code>(<code>NAME_COLLISION</code>)</li>
     *             <li>A container or element is read-only (
     *             <code>READ_ONLY</code>)</li>
     *             </ul>
     * 
     * @throws IllegalArgumentException
     *             any element or container is <code>null</code>
     */
    void move(IErlElement[] elements, IErlElement[] containers,
            IErlElement[] siblings, String[] renamings, boolean replace,
            IProgressMonitor monitor) throws ErlModelException;

    /**
     * Renames the given elements as specified. If one container is specified,
     * all elements are renamed within that container. If more than one
     * container is specified, the number of elements and containers must match,
     * and each element is renamed within its associated container.
     * 
     * @param elements
     *            the elements to rename
     * @param destinations
     *            the container, or list of containers
     * @param names
     *            the list of new names
     * @param replace
     *            <code>true</code> if an existing child in a target container
     *            with the target name should be replaced, and
     *            <code>false</code> to throw an exception in the event of a
     *            name collision
     * @param monitor
     *            a progress monitor
     * @throws ErlModelException
     *             if an element could not be renamed. Reasons include:
     *             <ul>
     *             <li>There is no element to process (NO_ELEMENTS_TO_PROCESS).
     *             The given elements is null or empty</li>
     *             <li>A specified element does not exist
     *             (ELEMENT_DOES_NOT_EXIST)</li>
     *             <li>A <code>CoreException</code> occurred while updating an
     *             underlying resource
     *             <li>A new name is invalid (<code>INVALID_NAME</code>)
     *             <li>A child already exists with the same name and
     *             <code>replace</code> has been specified as <code>false</code>( <code>NAME_COLLISION</code>)
     *             <li>An element is read-only (<code>READ_ONLY</code>)
     *             </ul>
     */
    void rename(IErlElement[] elements, IErlElement[] destinations,
            String[] names, boolean replace, IProgressMonitor monitor)
            throws ErlModelException;

    void notifyChange(IErlElement element);

    void addModelChangeListener(IErlModelChangeListener listener);

    void removeModelChangeListener(IErlModelChangeListener listener);

    IErlElement findElement(IResource resource);

    IErlElement findElement(IResource resource, boolean openElements);

    IErlProject findProject(IProject project);

    IErlModule findModule(IFile file);

    IErlModule findModule(String name);

    IErlModule findModuleExt(String name);

    public IErlElement innermostThat(final IErlElement el,
            final IErlangFirstThat firstThat);

    String getExternalModules(IErlProject erlProject);

    String getExternalIncludes(IErlProject erlProject);

    OtpErlangList getPathVars();

    /**
     * Locates definitions of functions matching the given signature. Function
     * name and module can be regexps.
     */
    IErlFunction findFunction(FunctionRef r);

    IErlProject newProject(final String name, final String path)
            throws ErlModelException;
}
