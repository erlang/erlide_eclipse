/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Eric Merritt - Api changes to integrate with Erlide
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.SelectionProviderAction;
import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * Superclass of all actions provided by the resource navigator.
 */
public abstract class ModuleNavigatorAction extends SelectionProviderAction {

	/**
	 * The module navigator
	 */
	private ModuleNavigator navigator;

	/**
	 * Creates a new instance of the class
	 * 
	 * @param lnavigator
	 *            the navigator parent
	 * @param label
	 *            the label
	 */
	public ModuleNavigatorAction(ModuleNavigator lnavigator, String label) {
		super(lnavigator.getViewer(), label);
		this.navigator = lnavigator;
	}

	/**
	 * Returns the module navigator for which this action was created.
	 * 
	 * @return ModuleNavigator The module navigator
	 */
	public ModuleNavigator getNavigator() {
		return navigator;
	}

	/**
	 * Returns the resource viewer
	 * 
	 * @return Viewer the module navigator's viewer
	 */
	protected Viewer getViewer() {
		return getNavigator().getViewer();
	}

	/**
	 * Returns the shell to use within actions.
	 * 
	 * @return Shell the navigator shell
	 */
	protected Shell getShell() {
		return getNavigator().getSite().getShell();
	}

	/**
	 * Returns the workbench.
	 * 
	 * @return IWorkbench the system workbench
	 */
	protected IWorkbench getWorkbench() {
		return PlatformUI.getWorkbench();
	}

	/**
	 * Returns the workbench window.
	 * 
	 * @return IWorkbenchWindow the system workbench window
	 */
	protected IWorkbenchWindow getWorkbenchWindow() {
		return getNavigator().getSite().getWorkbenchWindow();
	}
}
