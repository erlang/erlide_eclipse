/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.core.resources.IProject;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendListener;
import org.erlide.jinterface.ErlLogger;

public class BackendManagerPopup implements IBackendListener {

    private static final IBackendListener fInstance = new BackendManagerPopup();

    private BackendManagerPopup() {
    }

    public static void init() {
        BackendCore.getBackendManager().addBackendListener(fInstance);
    }

    @Override
    public void runtimeAdded(final IBackend b) {
        ErlLogger.debug("$$ added backend " + b);
        ErlLogger.debug("$$ added backend " + b.getRuntimeInfo());
        ErlLogger.debug("$$ added backend " + b.getRuntimeInfo().getName());
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final Display display = workbench.getDisplay();
        display.asyncExec(new Runnable() {

            @Override
            public void run() {
                // PopupDialog.showBalloon("Backend notification", "Added "
                // + b.getInfo().getName(), DELAY);
            }
        });
    }

    @Override
    public void runtimeRemoved(final IBackend b) {
        ErlLogger.debug("$$ removed backend " + b.getRuntimeInfo().getName());
        final IWorkbench workbench = PlatformUI.getWorkbench();
        final Display display = workbench.getDisplay();
        display.asyncExec(new Runnable() {

            @Override
            public void run() {
                // PopupDialog.showBalloon("Backend notification", "Removed "
                // + b.getInfo().getName(), DELAY);
            }
        });
    }

    @Override
    public void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName) {
    }
}
