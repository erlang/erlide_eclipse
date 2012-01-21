/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.console.actions;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.eclipse.debug.internal.ui.DebugPluginImages;
import org.eclipse.debug.internal.ui.IDebugHelpContextIds;
import org.eclipse.debug.internal.ui.IInternalDebugUIConstants;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleListener;
import org.eclipse.ui.console.IConsoleView;
import org.erlide.backend.IBackend;
import org.erlide.ui.console.ConsoleMessages;
import org.erlide.ui.console.ErlangConsole;

/**
 * ConsoleRemoveTerminatedAction
 */
@SuppressWarnings("restriction")
public class ConsoleRemoveLaunchAction extends Action implements
        IViewActionDelegate, IConsoleListener, ILaunchesListener2 {

    private ILaunch fLaunch;
    private IConsole fConsole;

    // only used when a view action delegate
    private IConsoleView fConsoleView;

    public ConsoleRemoveLaunchAction() {
        super(ConsoleMessages.ConsoleRemoveTerminatedAction_0);
        setToolTipText(ConsoleMessages.ConsoleRemoveTerminatedAction_1);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(this, IDebugHelpContextIds.CONSOLE_REMOVE_LAUNCH);
        setImageDescriptor(DebugPluginImages
                .getImageDescriptor(IDebugUIConstants.IMG_LCL_REMOVE));
        setDisabledImageDescriptor(DebugPluginImages
                .getImageDescriptor(IInternalDebugUIConstants.IMG_DLCL_REMOVE));
        setHoverImageDescriptor(DebugPluginImages
                .getImageDescriptor(IInternalDebugUIConstants.IMG_ELCL_REMOVE));
        DebugPlugin.getDefault().getLaunchManager().addLaunchListener(this);
        ConsolePlugin.getDefault().getConsoleManager().addConsoleListener(this);
    }

    public ConsoleRemoveLaunchAction(final ErlangConsole console) {
        this();
        fConsole = console;
        final IBackend backend = console.getBackend();
        fLaunch = backend.getLaunch();
        update();
    }

    public void dispose() {
        DebugPlugin.getDefault().getLaunchManager().removeLaunchListener(this);
        ConsolePlugin.getDefault().getConsoleManager()
                .removeConsoleListener(this);
    }

    public synchronized void update() {
        final ILaunch launch = getLaunch();
        if (launch != null) {
            setEnabled(launch.isTerminated());
        } else {
            setEnabled(false);
        }
    }

    @Override
    public synchronized void run() {
        final ILaunch launch = getLaunch();
        if (launch != null) {
            final ILaunchManager launchManager = DebugPlugin.getDefault()
                    .getLaunchManager();
            launchManager.removeLaunch(launch);
            ConsolePlugin.getDefault().getConsoleManager()
                    .removeConsoles(new IConsole[] { fConsole });
            fConsoleView = null;
            fLaunch = null;
        }
    }

    @Override
    public void init(final IViewPart view) {
        if (view instanceof IConsoleView) {
            fConsoleView = (IConsoleView) view;
        }
        update();
    }

    @Override
    public void run(final IAction action) {
        run();
    }

    @Override
    public void selectionChanged(final IAction action,
            final ISelection selection) {
    }

    @Override
    public void consolesAdded(final IConsole[] consoles) {
    }

    @Override
    public void consolesRemoved(final IConsole[] consoles) {
        update();
    }

    @Override
    public void launchesTerminated(final ILaunch[] launches) {
        update();
    }

    @Override
    public void launchesRemoved(final ILaunch[] launches) {
    }

    @Override
    public void launchesAdded(final ILaunch[] launches) {
    }

    @Override
    public void launchesChanged(final ILaunch[] launches) {
    }

    protected ILaunch getLaunch() {
        if (fConsoleView == null) {
            return fLaunch;
        }
        // else get dynamically, as this action was created via plug-in XML view
        // contribution
        final IConsole console = fConsoleView.getConsole();
        if (console instanceof ErlangConsole) {
            final ErlangConsole pconsole = (ErlangConsole) console;
            final IBackend backend = pconsole.getBackend();
            return backend.getLaunch();
        }
        return null;
    }
}
