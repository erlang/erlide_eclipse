/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.console.actions;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.internal.ui.DebugPluginImages;
import org.eclipse.debug.internal.ui.IDebugHelpContextIds;
import org.eclipse.debug.internal.ui.IInternalDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IUpdate;
import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.erlide.ui.console.ConsoleMessages;
import org.erlide.ui.console.ErlangConsole;

/**
 * ConsoleTerminateAction
 */
@SuppressWarnings("restriction")
public class ConsoleTerminateAction extends Action implements IUpdate {

    private ErlangConsole fConsole;

    /**
     * Creates a terminate action for the console
     */
    public ConsoleTerminateAction(final ErlangConsole fConsole2) {
        super(ConsoleMessages.ConsoleTerminateAction_0);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(this, IDebugHelpContextIds.CONSOLE_TERMINATE_ACTION);
        fConsole = fConsole2;
        setToolTipText(ConsoleMessages.ConsoleTerminateAction_1);
        setImageDescriptor(DebugPluginImages
                .getImageDescriptor(IInternalDebugUIConstants.IMG_LCL_TERMINATE));
        setDisabledImageDescriptor(DebugPluginImages
                .getImageDescriptor(IInternalDebugUIConstants.IMG_DLCL_TERMINATE));
        setHoverImageDescriptor(DebugPluginImages
                .getImageDescriptor(IInternalDebugUIConstants.IMG_LCL_TERMINATE));
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(this, IDebugHelpContextIds.CONSOLE_TERMINATE_ACTION);
        update();
    }

    @Override
    public void update() {
        final IBackend backend = fConsole.getBackend();
        setEnabled(backend.isManaged() && !backend.isStopped()
                && backend != BackendCore.getBackendManager().getIdeBackend());
    }

    @Override
    public void run() {
        try {
            final IBackend backend = fConsole.getBackend();
            final ILaunch launch = backend.getLaunch();
            if (launch != null) {
                terminate(launch);

                setEnabled(false);
                fConsole.stop();
            }
            BackendCore.getBackendManager().dispose(backend);
        } catch (final DebugException e) {
            // TODO: report exception
        }
    }

    private void terminate(final ILaunch launch) throws DebugException {
        final IDebugTarget[] debugTargets = launch.getDebugTargets();
        for (final IDebugTarget target : debugTargets) {
            if (target.canTerminate()) {
                target.terminate();
            }
        }
        if (launch.canTerminate()) {
            launch.terminate();
        }
    }

    public void dispose() {
        fConsole = null;
    }

}
