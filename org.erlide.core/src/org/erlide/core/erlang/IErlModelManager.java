/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang;

import java.util.HashSet;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.internal.ErlElement;
import org.erlide.core.erlang.internal.ErlModel;
import org.erlide.core.erlang.util.ElementChangedEvent;
import org.erlide.core.erlang.util.IElementChangedListener;

public interface IErlModelManager extends ISaveParticipant {

	/**
	 * Returns the Erlang element corresponding to the given resource, or
	 * <code>null</code> if unable to associate the given resource with a
	 * Erlang element.
	 * <p>
	 * The resource must be one of:
	 * <ul>
	 * <li>a project - the element returned is the corresponding
	 * <code>IErlProject</code> </li>
	 * <li>a <code>.erl</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.beam</code> file - the element returned is the
	 * corresponding <code>IErlBeamFile</code></li>
	 * <li>a folder - the element returned is the corresponding
	 * <code>IErlPackage</code> -- not implemented yet</li>
	 * <li>the workspace root resource - the element returned is the
	 * <code>IErlModel</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	IErlElement create(IResource resource, IErlProject project);

	/**
	 * Returns the Erlang element corresponding to the given file, its project
	 * being the given project. Returns <code>null</code> if unable to
	 * associate the given file with a Erlang element.
	 * 
	 * <p>
	 * The file must be one of:
	 * <ul>
	 * <li>a <code>.erl</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.beam</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	IErlElement create(IFile file, IErlProject project);

	/**
	 * Returns the package fragment or package fragment root corresponding to
	 * the given folder, its parent or great parent being the given project. or
	 * <code>null</code> if unable to associate the given folder with a Erlang
	 * element.
	 * <p>
	 * Note that a package fragment root is returned rather than a default
	 * package.
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 */
	IErlElement create(IFolder folder, IErlProject project);

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#createModuleFrom(org.eclipse.core.resources.IFile,
	 *      org.erlide.core.erlang.IErlProject)
	 */
	IErlModule createModuleFrom(IFile file, IErlProject project);

	/**
	 * Create a module given name, text and project
	 * <p>
	 * Currently used by compare functions
	 */
	public IErlModule createModuleFrom(String name, String text,
			IErlProject project, IFile file);

	/**
	 * Returns the Erlang project corresponding to the given project.
	 * <p>
	 * Creating a Erlang Project has the side effect of creating and opening all
	 * of the project's parents if they are not yet open.
	 * <p>
	 * Note that no check is done at this time on the existence or the Erlang
	 * nature of this project.
	 * 
	 * @param project
	 *            the given project
	 * @return the Erlang project corresponding to the given project, null if
	 *         the given project is null
	 */
	IErlProject create(IProject project);

	IErlProject createEmptyProject();

	/**
	 * Returns the Erlang element corresponding to the given file, or
	 * <code>null</code> if unable to associate the given file with a Erlang
	 * element.
	 * 
	 * <p>
	 * The file must be one of:
	 * <ul>
	 * <li>a <code>.Erlang</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.class</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * <li>a <code>.jar</code> file - the element returned is the
	 * corresponding <code>IPackageFragmentRoot</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 * 
	 * @param file
	 *            the given file
	 * @return the Erlang element corresponding to the given file, or
	 *         <code>null</code> if unable to associate the given file with a
	 *         Erlang element
	 */
	IErlElement create(IFile file);

	/**
	 * Returns the package fragment or package fragment root corresponding to
	 * the given folder, or <code>null</code> if unable to associate the given
	 * folder with a Erlang element.
	 * <p>
	 * Note that a package fragment root is returned rather than a default
	 * package.
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 * 
	 * @param folder
	 *            the given folder
	 * @return the package fragment or package fragment root corresponding to
	 *         the given folder, or <code>null</code> if unable to associate
	 *         the given folder with a Erlang element
	 */
	IErlElement create(IFolder folder);

	/**
	 * Returns the Erlang element corresponding to the given resource, or
	 * <code>null</code> if unable to associate the given resource with a
	 * Erlang element.
	 * <p>
	 * The resource must be one of:
	 * <ul>
	 * <li>a project - the element returned is the corresponding
	 * <code>IErlProject</code></li>
	 * <li>a <code>.Erlang</code> file - the element returned is the
	 * corresponding <code>IErlModule</code></li>
	 * <li>a <code>.class</code> file - the element returned is the
	 * corresponding <code>IClassFile</code></li>
	 * <li>a <code>.jar</code> file - the element returned is the
	 * corresponding <code>IPackageFragmentRoot</code></li>
	 * <li>a folder - the element returned is the corresponding
	 * <code>IPackageFragmentRoot</code> or <code>IPackageFragment</code>
	 * </li>
	 * <li>the workspace root resource - the element returned is the
	 * <code>IErlModel</code></li>
	 * </ul>
	 * <p>
	 * Creating a Erlang element has the side effect of creating and opening all
	 * of the element's parents if they are not yet open.
	 * 
	 * @param resource
	 *            the given resource
	 * @return the Erlang element corresponding to the given resource, or
	 *         <code>null</code> if unable to associate the given resource
	 *         with a Erlang element
	 */
	IErlElement create(IResource resource);

	/**
	 * Returns element from path
	 * 
	 * @param path
	 * @return IErlElement
	 */
	public IErlElement create(IPath path);

	/**
	 * Returns the Erlang model.
	 * 
	 * @param root
	 *            the given root
	 * @return the Erlang model, or <code>null</code> if the root is null
	 */
	IErlModel create(IWorkspaceRoot root);

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getInfo(org.erlide.core.erlang.IErlElement)
	 */
	Object getInfo(IErlElement element);

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getErlangModel()
	 */
	ErlModel getErlangModel();

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#getLastBuiltState(org.eclipse.core.resources.IProject,
	 *      org.eclipse.core.runtime.IProgressMonitor)
	 */
	Object getLastBuiltState(IProject project, IProgressMonitor monitor);

	/*
	 * Removes all cached info for the given element (including all children)
	 * from the cache. Returns the info for the given element, or null if it was
	 * closed.
	 */
	/**
	 * @see org.erlide.core.erlang.IErlModelManager#removeInfoAndChildren(org.erlide.core.erlang.internal.ErlElement)
	 */
	Object removeInfoAndChildren(ErlElement element) throws ErlModelException;

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#setLastBuiltState(org.eclipse.core.resources.IProject,
	 *      java.lang.Object)
	 */
	void setLastBuiltState(IProject project, Object state);

	/**
	 * @see org.erlide.core.erlang.IErlModelManager#shutdown()
	 */
	void shutdown();

	/**
	 * Adds the given listener for changes to Erlang elements. Has no effect if
	 * an identical listener is already registered. After completion of this
	 * method, the given listener will be registered for exactly the specified
	 * events. If they were previously registered for other events, they will be
	 * deregistered.
	 * <p>
	 * Once registered, a listener starts receiving notification of changes to
	 * Erlang elements in the model. The listener continues to receive
	 * notifications until it is replaced or removed.
	 * </p>
	 * <p>
	 * Listeners can listen for several types of event as defined in
	 * <code>ElementChangeEvent</code>. Clients are free to register for any
	 * number of event types however if they register for more than one, it is
	 * their responsibility to ensure they correctly handle the case where the
	 * same Erlang element change shows up in multiple notifications. Clients
	 * are guaranteed to receive only the events for which they are registered.
	 * </p>
	 * 
	 * @param listener
	 *            the listener
	 * @param eventMask
	 *            the bit-wise OR of all event types of interest to the listener
	 * @see IElementChangedListener
	 * @see ElementChangedEvent
	 * @see #removeElementChangedListener(IElementChangedListener)
	 */
	void addElementChangedListener(IElementChangedListener listener,
			int eventMask);

	/**
	 * Removes the given element changed listener. Has no affect if an identical
	 * listener is not registered.
	 * 
	 * @param listener
	 *            the listener
	 */
	void removeElementChangedListener(IElementChangedListener listener);

	/**
	 * Returns a new empty region.
	 * 
	 * @return a new empty region
	 */
	IRegion newRegion();

	/**
	 * Adds the given listener for changes to Erlang elements. Has no effect if
	 * an identical listener is already registered.
	 * 
	 * This listener will only be notified during the POST_CHANGE resource
	 * change notification and any reconcile operation (POST_RECONCILE). For
	 * finer control of the notification, use
	 * <code>addElementChangedListener(IElementChangedListener,int)</code>,
	 * which allows to specify a different eventMask.
	 * 
	 * @param listener
	 *            the listener
	 * @see ElementChangedEvent
	 */
	void addElementChangedListener(IElementChangedListener listener);

	/**
	 * @see org.erlide.core.erlang.ErlModelManager#getOptionNames()
	 */
	HashSet<String> getOptionNames();

	Map<IErlElement, IErlElement> getElementsOutOfSynchWithBuffers();

	/**
	 * @see org.erlide.core.erlang.ErlModelManager#registerErlModelDelta(IErlElementDelta)
	 */
	void registerErlModelDelta(IErlElementDelta delta);

	/**
	 * @see org.erlide.core.erlang.ErlModelManager#fire(int)
	 */

	void fire(int post_change);

}
