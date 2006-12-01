/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Eric Merritt - API Changes for erlide
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import org.eclipse.core.resources.IContainer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ResourceListSelectionDialog;
import org.erlide.ui.ErlideUIPlugin;

/**
 * Shows a list of resources to the user with a text entry field for a string
 * pattern used to filter the list of resources.
 * 
 */
class GotoResourceDialog extends ResourceListSelectionDialog {

	/**
	 * Creates a new instance of the class.
	 * 
	 * @param parentShell
	 * @param container
	 * @param typesMask
	 */
	protected GotoResourceDialog(Shell parentShell, IContainer container,
			int typesMask) {
		super(parentShell, container, typesMask);
		setTitle(ErlideUIPlugin
				.getResourceString("views.modules.actions.goto.title"));
	}
}
