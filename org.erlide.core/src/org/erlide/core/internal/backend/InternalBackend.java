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
package org.erlide.core.internal.backend;

import org.eclipse.core.runtime.Assert;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.IErlRuntime;

public class InternalBackend extends Backend {

    public InternalBackend(final BackendData data, final IErlRuntime runtime)
            throws BackendException {
        super(data, runtime);
        Assert.isLegal(launch == null);
    }

}
