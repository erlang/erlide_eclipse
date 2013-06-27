/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.backend.internal;

import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.BackendException;
import org.erlide.backend.api.IBackendManager;
import org.erlide.runtime.api.ICodeBundle;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.util.ErlLogger;

public class InternalBackend extends Backend {

    public InternalBackend(final BackendData data, final IErlRuntime runtime,
            final IBackendManager backendManager) throws BackendException {
        super(data, runtime, backendManager);
    }

    @SuppressWarnings("unused")
    @Override
    public void runtimeDown(final IErlRuntime runtime) {
        getData().setLaunch(null);
        // TODO fix this
        if (false && getData().isRestartable()) {
            ErlLogger.debug("restart %s", getName());

            // TODO remove code duplication here
            // connect();
            for (final ICodeBundle bb : backendManager.getCodeBundles()
                    .values()) {
                registerCodeBundle(bb);
            }
            startErlangApps(getRuntime().getEventMbox().self(), true);
        }
    }
}
