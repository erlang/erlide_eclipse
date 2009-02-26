/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import org.eclipse.jface.action.Action;
import org.eclipse.ui.PlatformUI;

/**
 * Toggles console auto-scroll
 */
public class ScrollLockAction extends Action {

	private TestRunnerViewPart fRunnerViewPart;

	public ScrollLockAction(TestRunnerViewPart viewer) {
		super(GUnitMessages.ScrollLockAction_action_label);
		this.fRunnerViewPart = viewer;
		setToolTipText(GUnitMessages.ScrollLockAction_action_tooltip);
		setDisabledImageDescriptor(GUnitPlugin
				.getImageDescriptor("dlcl16/lock.gif")); //$NON-NLS-1$
		setHoverImageDescriptor(GUnitPlugin
				.getImageDescriptor("elcl16/lock.gif")); //$NON-NLS-1$
		setImageDescriptor(GUnitPlugin.getImageDescriptor("elcl16/lock.gif")); //$NON-NLS-1$
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IGUnitHelpContextIds.OUTPUT_SCROLL_LOCK_ACTION);
		setChecked(false);
	}

	/**
	 * @see org.eclipse.jface.action.IAction#run()
	 */
	@Override
	public void run() {
		this.fRunnerViewPart.setAutoScroll(!isChecked());
	}
}
