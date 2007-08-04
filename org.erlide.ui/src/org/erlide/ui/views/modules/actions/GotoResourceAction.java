/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Eric Merritt - API changes for erlide
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.StructuredSelection;
import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * Implements the go to resource action. Opens a dialog and set the navigator
 * selection with the resource selected by the user.
 */
public class GotoResourceAction extends ModuleNavigatorAction {

	/**
	 * Creates a new instance of the class.
	 * 
	 * @param lnavigator
	 *            the navigator
	 * @param label
	 *            the system label
	 */
	public GotoResourceAction(ModuleNavigator lnavigator, String label) {
		super(lnavigator, label);
	}

	/**
	 * Collect all resources in the workbench open a dialog asking the user to
	 * select a resource and change the selection in the navigator.
	 */
	@Override
	public void run() {
		final IContainer container = (IContainer) getViewer().getInput();
		final GotoResourceDialog dialog = new GotoResourceDialog(getShell(),
				container, IResource.FILE | IResource.FOLDER |
						IResource.PROJECT);
		dialog.open();
		final Object[] result = dialog.getResult();
		if (result == null || result.length == 0 ||
				!(result[0] instanceof IResource)) {
			return;
		}

		final IResource selection = (IResource) result[0];
		getViewer().setSelection(new StructuredSelection(selection), true);
	}
}
