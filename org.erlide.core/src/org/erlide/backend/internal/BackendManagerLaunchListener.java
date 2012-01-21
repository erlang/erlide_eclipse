/*******************************************************************************
 * Copyright (c) 2011 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.internal;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.erlide.backend.IBackendManager;

public class BackendManagerLaunchListener implements ILaunchesListener2 {

    private final IBackendManager backendManager;
    private final ILaunchManager launchManager;

    public BackendManagerLaunchListener(final IBackendManager backendManager,
            final ILaunchManager launchManager) {
        this.backendManager = backendManager;
        this.launchManager = launchManager;
        launchManager.addLaunchListener(this);
    }

    public void dispose() {
        launchManager.removeLaunchListener(this);
    }

    @Override
    public void launchesAdded(final ILaunch[] launches) {
    }

    @Override
    public void launchesTerminated(final ILaunch[] launches) {
        for (final ILaunch launch : launches) {
            backendManager.terminateBackendsForLaunch(launch);
        }
    }

    @Override
    public void launchesRemoved(final ILaunch[] launches) {
        for (final ILaunch launch : launches) {
            backendManager.removeBackendsForLaunch(launch);
        }
    }

    @Override
    public void launchesChanged(final ILaunch[] launches) {
    }

}
