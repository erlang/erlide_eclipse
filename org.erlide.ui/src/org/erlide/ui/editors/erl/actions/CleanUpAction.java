/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Alain O'Dea
 *******************************************************************************/
package org.erlide.ui.editors.erl.actions;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.Action;
import org.erlide.core.services.cleanup.CleanUpProviders;
import org.erlide.jinterface.ErlLogger;

/*******************************************************************************
 * <p>
 * The {@link CleanUpAction} class provides the {@link Action} behind Erlang
 * module source code clean up.
 * </p>
 * 
 * @author Alain O'Dea [alain dot odea at gmail dot com]
 *******************************************************************************/
public class CleanUpAction extends Action {

    /**
     * <p>
     * Erlang module to be acted upon.
     * </p>
     */
    private final IResource resource;

    /**
     * <p>
     * Construct a {@link CleanUpAction} for a particular {@link IResource} for
     * an Erlang module.
     * </p>
     * 
     * @param resource
     *            {@link IResource} for a an Erlang
     */
    public CleanUpAction(final IResource resource) {
        super("Clean Up...");
        this.resource = resource;
    }

    @Override
    public void run() {
        try {
            CleanUpProviders.createForIResource(resource).cleanUp();
        } catch (final Exception e) {
            ErlLogger.debug(e);
        }
    }

}
