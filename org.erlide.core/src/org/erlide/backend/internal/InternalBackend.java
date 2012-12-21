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
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackendData;
import org.erlide.backend.IBackendManager;
import org.erlide.runtime.IErlRuntime;

public class InternalBackend extends Backend {

    public InternalBackend(final IBackendData data, final IErlRuntime runtime,
            final IBackendManager backendManager) throws BackendException {
        super(data, runtime, backendManager);
        Assert.isLegal(getLaunch() == null);
    }

}
