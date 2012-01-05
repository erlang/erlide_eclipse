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

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.ErlangToolkit;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlParser;
import org.erlide.core.model.util.ElementChangedEvent;
import org.erlide.core.model.util.IElementChangedListener;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.base.Predicate;

/**
 * Represent the root Erlang element corresponding to the workspace. Since there
 * is only one such root element, it is commonly referred to as <em>the</em>
 * Erlang model element. The Erlang model element needs to be opened before it
 * can be navigated or manipulated. The Erlang model element has no parent (it
 * is the root of the Erlang element hierarchy). Its children are
 * <code>IErlProject</code>s.
 * <p>
 * This interface is not intended to be implemented by clients. An instance of
 * one of these handles can be created via
 * <code>ErlangCore.create(workspace.getRoot())</code>.
 * </p>
 * 
 * @see ErlangCore#create(org.eclipse.core.resources.IWorkspaceRoot)
 */
public interface IErlModel extends IErlElement, IOpenable, IParent,
        IErlElementLocator {

    /**
     * Returns the Erlang project with the given name. This is a handle-only
     * method. The project may or may not exist.
     * 
     * @param project
     *            the name of the Erlang project
     * @return the Erlang project with the given name
     */
    IErlProject getErlangProject(IProject project);

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

    void addModelChangeListener(IErlModelChangeListener listener);

    void removeModelChangeListener(IErlModelChangeListener listener);

    IErlElement innermostThat(final IErlElement el,
            final Predicate<IErlElement> firstThat);

    OtpErlangList getPathVars();

    IErlProject newProject(final String name, final String path)
            throws ErlModelException;

    void notifyChange(IErlElement element);

    /**
     * Returns the Erlang element corresponding to the given file, its project
     * being the given project. Returns <code>null</code> if unable to associate
     * the given file with a Erlang element.
     * <p>
     * The file must be one of:
     * <ul>
     * <li>a <code>.erl</code> file - the element returned is the corresponding
     * <code>IErlModule</code></li>
     * </ul>
     * <p>
     * Creating a Erlang element has the side effect of creating and opening all
     * of the element's parents if they are not yet open.
     */
    IErlElement create(IResource resource);

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

    void registerModelDelta(IErlElementDelta delta);

    IErlModule getModuleFromFile(IParent parent, String name,
            String initialText, String path, String key);

    IErlModule getModuleFromText(IParent parent, String name,
            String initialText, String key);

    public void removeModule(final IErlModule module);

    void putEdited(String path, IErlModule module);

    IErlParser getParser();

    ErlangToolkit getToolkit();
}
