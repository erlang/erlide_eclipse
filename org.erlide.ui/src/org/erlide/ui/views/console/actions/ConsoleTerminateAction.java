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
package org.erlide.ui.views.console.actions;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.internal.ui.DebugPluginImages;
import org.eclipse.debug.internal.ui.IDebugHelpContextIds;
import org.eclipse.debug.internal.ui.IInternalDebugUIConstants;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IUpdate;
import org.erlide.ui.views.console.ErlangConsole;

/**
 * ConsoleTerminateAction
 */
public class ConsoleTerminateAction extends Action implements IUpdate {

	private ErlangConsole fConsole;

	/**
	 * Creates a terminate action for the console
	 */
	public ConsoleTerminateAction(ErlangConsole fConsole2) {
		super(ConsoleMessages.ConsoleTerminateAction_0);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IDebugHelpContextIds.CONSOLE_TERMINATE_ACTION);
		fConsole = fConsole2;
		setToolTipText(ConsoleMessages.ConsoleTerminateAction_1);
		setImageDescriptor(DebugPluginImages
				.getImageDescriptor(IInternalDebugUIConstants.IMG_LCL_TERMINATE));
		setDisabledImageDescriptor(DebugPluginImages
				.getImageDescriptor(IInternalDebugUIConstants.IMG_DLCL_TERMINATE));
		setHoverImageDescriptor(DebugPluginImages
				.getImageDescriptor(IInternalDebugUIConstants.IMG_LCL_TERMINATE));
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IDebugHelpContextIds.CONSOLE_TERMINATE_ACTION);
		update();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.texteditor.IUpdate#update()
	 */
	public void update() {
		// IProcess process = fConsole.getBackend();
		// setEnabled(process.canTerminate());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.action.IAction#run()
	 */
	@Override
	public void run() {
		// FIXME
		// try {
		// final ErlideBackend backend = fConsole.getBackend();
		// ErlangCore.getBackendManager().dispose(backend);
		// ILaunch launch = backend.getLaunch();
		// if (launch != null) {
		// killTargets(launch);
		// launch.terminate();
		// }
		// } catch (DebugException e) {
		// // TODO: report exception
		// }
	}

	private void killTargets(ILaunch launch) throws DebugException {
		IDebugTarget[] debugTargets = launch.getDebugTargets();
		for (IDebugTarget target : debugTargets) {
			if (target.canTerminate()) {
				target.terminate();
			}
		}
	}

	public void dispose() {
		fConsole = null;
	}

}
