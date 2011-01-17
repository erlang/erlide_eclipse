/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.erlang;

import java.util.HashSet;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.erlang.util.ElementChangedEvent;
import org.erlide.core.erlang.util.IElementChangedListener;

public interface IErlModelManager extends ISaveParticipant {

    /**
     * Returns the Erlang element corresponding to the given file, its project
     * being the given project. Returns <code>null</code> if unable to associate
     * the given file with a Erlang element.
     * 
     * <p>
     * The file must be one of:
     * <ul>
     * <li>a <code>.erl</code> file - the element returned is the corresponding
     * <code>IErlModule</code></li>
     * <li>a <code>.beam</code> file - the element returned is the corresponding
     * <code>IClassFile</code></li>
     * </ul>
     * <p>
     * Creating a Erlang element has the side effect of creating and opening all
     * of the element's parents if they are not yet open.
     */
    IErlElement create(IResource resource, IParent parent);

    /**
     * Create a reference OTP project
     * 
     * @throws CoreException
     */
    IErlProject createOtpProject(IProject project) throws CoreException;

    Object getInfo(IErlElement element);

    IErlModel getErlangModel();

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

    /**
     * @see org.erlide.core.erlang.ErlModelManager#registerModelDelta(IErlElementDelta)
     */
    void registerModelDelta(IErlElementDelta delta);

    /**
     * @see org.erlide.core.erlang.ErlModelManager#fire(int)
     */

    void fire(int post_change);

    IErlModule getModuleFromFile(IParent parent, String name,
            String initialText, String path, String key);

    IErlModule getModuleFromText(IParent parent, String name,
            String initialText, String key);

    public void removeModule(final IErlModule module);

}
