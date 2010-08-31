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
import org.erlide.jinterface.backend.Backend;
import org.erlide.runtime.backend.ErlideBackend;
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
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IDebugHelpContextIds.CONSOLE_REMOVE_LAUNCH);
		setImageDescriptor(DebugPluginImages
				.getImageDescriptor(IDebugUIConstants.IMG_LCL_REMOVE));
		setDisabledImageDescriptor(DebugPluginImages
				.getImageDescriptor(IInternalDebugUIConstants.IMG_DLCL_REMOVE));
		setHoverImageDescriptor(DebugPluginImages
				.getImageDescriptor(IInternalDebugUIConstants.IMG_ELCL_REMOVE));
		DebugPlugin.getDefault().getLaunchManager().addLaunchListener(this);
		ConsolePlugin.getDefault().getConsoleManager().addConsoleListener(this);
	}

	public ConsoleRemoveLaunchAction(ErlangConsole console) {
		this();
		this.fConsole = console;
		Backend backend = console.getBackend();
		if (backend instanceof ErlideBackend) {
			ErlideBackend eb = (ErlideBackend) backend;
			fLaunch = eb.getLaunch();
		}
		update();
	}

	public void dispose() {
		DebugPlugin.getDefault().getLaunchManager().removeLaunchListener(this);
		ConsolePlugin.getDefault().getConsoleManager().removeConsoleListener(
				this);
	}

	public synchronized void update() {
		ILaunch launch = getLaunch();
		if (launch != null) {
			setEnabled(launch.isTerminated());
		} else {
			setEnabled(false);
		}
	}

	@Override
	public synchronized void run() {
		ILaunch launch = getLaunch();
		if (launch != null) {
			ILaunchManager launchManager = DebugPlugin.getDefault()
					.getLaunchManager();
			launchManager.removeLaunch(launch);
			ConsolePlugin.getDefault().getConsoleManager().removeConsoles(
					new IConsole[] { fConsole });
			fConsoleView = null;
			fLaunch = null;
		}
	}

	public void init(IViewPart view) {
		if (view instanceof IConsoleView) {
			fConsoleView = (IConsoleView) view;
		}
		update();
	}

	public void run(IAction action) {
		run();
	}

	public void selectionChanged(IAction action, ISelection selection) {
	}

	public void consolesAdded(IConsole[] consoles) {
	}

	public void consolesRemoved(IConsole[] consoles) {
		update();
	}

	public void launchesTerminated(ILaunch[] launches) {
		update();
	}

	public void launchesRemoved(ILaunch[] launches) {
	}

	public void launchesAdded(ILaunch[] launches) {
	}

	public void launchesChanged(ILaunch[] launches) {
	}

	protected ILaunch getLaunch() {
		if (fConsoleView == null) {
			return fLaunch;
		}
		// else get dynamically, as this action was created via plug-in XML view
		// contribution
		IConsole console = fConsoleView.getConsole();
		if (console instanceof ErlangConsole) {
			ErlangConsole pconsole = (ErlangConsole) console;
			Backend backend = pconsole.getBackend();
			if (backend instanceof ErlideBackend) {
				ErlideBackend eb = (ErlideBackend) backend;
				return eb.getLaunch();
			}
		}
		return null;
	}
}
