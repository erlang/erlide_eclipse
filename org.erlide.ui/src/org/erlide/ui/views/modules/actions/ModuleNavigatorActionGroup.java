/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.ui.views.modules.actions;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.ui.actions.ActionGroup;
import org.erlide.ui.views.modules.ModuleNavigator;

/**
 * Handle module action creation grouping
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public abstract class ModuleNavigatorActionGroup extends ActionGroup {

	/**
	 * The resource navigator.
	 */
	protected ModuleNavigator navigator;

	/**
	 * Constructs a new navigator action group and creates its actions.
	 * 
	 * @param lnavigator
	 *            the resource navigator
	 */
	public ModuleNavigatorActionGroup(ModuleNavigator lnavigator) {
		this.navigator = lnavigator;
		makeActions();
	}

	/**
	 * Returns the resource navigator.
	 * 
	 * @return the navigator to return
	 */
	public ModuleNavigator getNavigator() {
		return navigator;
	}

	/**
	 * Handles a key pressed event by invoking the appropriate action. Does
	 * nothing by default.
	 * 
	 * @param event
	 *            The key event
	 */
	public void handleKeyPressed(KeyEvent event) {
		// nothing
	}

	/**
	 * Makes the actions contained in this action group.
	 */
	protected abstract void makeActions();

	/**
	 * Runs the default action in the group. Does nothing by default.
	 * 
	 * @param selection
	 *            the current selection
	 */
	public void runDefaultAction(IStructuredSelection selection) {
		// nothing
	}
}
