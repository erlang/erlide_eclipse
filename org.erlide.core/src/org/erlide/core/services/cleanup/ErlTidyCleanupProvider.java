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
package org.erlide.core.services.cleanup;

import org.eclipse.core.resources.IResource;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.jinterface.rpc.IRpcFuture;

/**
 * <p>
 * The {@link ErlTidyCleanupProvider} class provides an implementation of
 * {@link CleanUpProvider} that uses <code>erl_tidy</code> to do the heavy
 * lifting.
 * </p>
 * 
 * @author Alain O'Dea [alain dot odea at gmail dot com]
 */
class ErlTidyCleanupProvider implements CleanUpProvider {

    /**
     * <p>
     * Expected limit of the user's patience in milli-seconds.
     * </p>
     */
    private static final long PATIENCE_LIMIT = 10000;

    /**
     * <p>
     * Erlang module to clean up.
     * </p>
     */
    private final IResource resource;

    /**
     * <p>
     * Construct an {@link ErlTidyCleanupProvider} for a particular
     * {@link IResource} for an Erlang module.
     * </p>
     * 
     * @param resource
     *            {@link IResource} for an Erlang module
     */
    public ErlTidyCleanupProvider(final IResource resource) {
        this.resource = resource;
    }

    @Override
    public void cleanUp() throws Exception {
        // lookup a backend to run erl_tidy with
        final IBackend Backend = BackendCore.getBackendManager()
                .getIdeBackend();

        // invoke erl_tidy in the background
        final String absolutePathToErlangModule = resource.getLocation()
                .toString();
        final IRpcFuture erlTidyFuture = Backend.async_call("erl_tidy", "file",
                "s", absolutePathToErlangModule);

        // wait as long as reasonable for erl_tidy to finish
        erlTidyFuture.get(PATIENCE_LIMIT);

        // refresh the resource so it reflects the altered state on disk
        resource.refreshLocal(IResource.DEPTH_ZERO, null);
    }

}
