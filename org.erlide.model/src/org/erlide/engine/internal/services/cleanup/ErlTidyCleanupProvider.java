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
package org.erlide.engine.internal.services.cleanup;

import java.util.concurrent.TimeUnit;

import org.eclipse.core.resources.IResource;
import org.erlide.engine.services.cleanup.CleanupProvider;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcFuture;

/**
 * <p>
 * The {@link ErlTidyCleanupProvider} class provides an implementation of
 * {@link CleanupProvider} that uses <code>erl_tidy</code> to do the heavy
 * lifting.
 * </p>
 *
 * @author Alain O'Dea [alain dot odea at gmail dot com]
 */
public class ErlTidyCleanupProvider implements CleanupProvider {

    /**
     * <p>
     * Expected limit of the user's patience in milli-seconds.
     * </p>
     */
    private static final long PATIENCE_LIMIT = 10000;

    private final IOtpRpc backend;

    /**
     * <p>
     * Construct an {@link ErlTidyCleanupProvider} for a particular
     * {@link IResource} for an Erlang module.
     * </p>
     *
     * @param backend
     *
     * @param resource
     *            {@link IResource} for an Erlang module
     */
    public ErlTidyCleanupProvider(final IOtpRpc backend) {
        this.backend = backend;
    }

    @Override
    public void cleanUp(final IResource resource) throws Exception {
        // invoke erl_tidy in the background
        final String absolutePathToErlangModule = resource.getLocation().toString();
        final RpcFuture erlTidyFuture = backend.async_call("erl_tidy", "file", "s",
                absolutePathToErlangModule);

        // wait as long as reasonable for erl_tidy to finish
        erlTidyFuture.get(PATIENCE_LIMIT, TimeUnit.MILLISECONDS);

        // refresh the resource so it reflects the altered state on disk
        resource.refreshLocal(IResource.DEPTH_ZERO, null);
    }

}
