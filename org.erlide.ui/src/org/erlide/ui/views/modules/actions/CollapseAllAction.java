/******************************************************************************* 
 * Copyright (c) 2000, 2003 IBM Corporation and others. 
 * All rights reserved. This program and the accompanying materials! 
 * are made available under the terms of the Eclipse Public License v1.0 
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 *   IBM Corporation - initial API and implementation 
 *   Sebastian Davids <sdavids@gmx.de> - Collapse all action (25826)
 *   Eric Merritt - Api changes for erlang
 ******************************************************************************/
package org.erlide.ui.views.modules.actions;

import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * Collapse all project nodes.
 */
public class CollapseAllAction extends ModuleNavigatorAction {

	/**
	 * Creates the action.
	 * 
	 * @param lnavigator
	 *            the module navigator
	 * @param label
	 *            the label for the action
	 */
	public CollapseAllAction(ModuleNavigator lnavigator, String label) {
		super(lnavigator, label);
		setEnabled(true);
	}

	/**
	 * Implementation of method defined on <code>IAction</code>.
	 */
	@Override
	public void run() {
		getNavigator().getViewer().collapseAll();
	}
}
