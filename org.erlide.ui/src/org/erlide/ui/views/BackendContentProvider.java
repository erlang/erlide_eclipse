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
package org.erlide.ui.views;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;

public class BackendContentProvider implements IStructuredContentProvider {

    @Override
    public void dispose() {
        // TODO unsubscribe from backend manager

    }

    @Override
    public void inputChanged(final Viewer vwr, final Object oldInput,
            final Object newInput) {
        // TODO subscribe to backendmanager events
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        final Collection<IBackend> bs = BackendCore.getBackendManager()
                .getAllBackends();
        return bs.toArray(new IBackend[bs.size()]);
    }
}
