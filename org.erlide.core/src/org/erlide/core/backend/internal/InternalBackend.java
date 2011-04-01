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
package org.erlide.core.backend.internal;

import org.eclipse.core.runtime.Assert;
import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.BackendException;

public class InternalBackend extends Backend {

    public InternalBackend(final BackendData data) throws BackendException {
        super(data);
        Assert.isLegal(launch == null);
    }

}
