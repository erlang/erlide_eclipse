/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Eric Merritt - Api changes for erlide
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * This is the action group for the goto actions.
 */
public class GotoActionGroup extends ModuleNavigatorActionGroup {

	/**
	 * open the resource
	 */
	private GotoResourceAction goToResourceAction;

	/**
	 * The group constructor
	 * 
	 * @param lnavigator
	 */
	public GotoActionGroup(ModuleNavigator lnavigator) {
		super(lnavigator);
	}

	/**
	 * Fill up the action bars
	 * 
	 * @see org.eclipse.ui.actions.ActionGroup#fillActionBars(org.eclipse.ui.IActionBars)
	 */
	@Override
	public void fillActionBars(IActionBars actionBars) {
		actionBars.setGlobalActionHandler(
				IWorkbenchActionConstants.GO_TO_RESOURCE, goToResourceAction);
	}

	/**
	 * Make the actions
	 * 
	 * @see org.erlide.ui.views.modules.actions.ModuleNavigatorActionGroup#makeActions()
	 */
	@Override
	protected void makeActions() {

		goToResourceAction = new GotoResourceAction(navigator, ErlideUIPlugin
				.getResourceString("views.modules.actions.goto.resourcelabel"));
	}

}
