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

import org.eclipse.core.runtime.Assert;
import org.erlide.backend.BackendData;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackendManager;
import org.erlide.runtime.ICodeBundle;
import org.erlide.runtime.IErlRuntime;
import org.erlide.utils.ErlLogger;

public class InternalBackend extends Backend {

    public InternalBackend(final BackendData data, final IErlRuntime runtime,
            final IBackendManager backendManager) throws BackendException {
        super(data, runtime, backendManager);
        Assert.isLegal(getLaunch() == null);
    }

    @Override
    public void runtimeDown(final IErlRuntime runtime) {
        runtime.restart();
        ErlLogger.debug("restart %s", getName());

        // TODO remove code duplication here
        connect();
        for (final ICodeBundle bb : backendManager.getCodeBundles().values()) {
            registerCodeBundle(bb);
        }
        startErlangApps(getEventPid(), true);
    }
}
