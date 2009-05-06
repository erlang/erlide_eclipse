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
package org.erlide.gunit.internal.ui;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.erlide.gunit.internal.model.TestElement;

/**
 * Action to enable/disable stack trace filtering.
 */
public class CompareResultsAction extends Action {

	private final FailureTrace fView;

	private CompareResultDialog fOpenDialog;

	public CompareResultsAction(final FailureTrace view) {
		super(GUnitMessages.CompareResultsAction_label);
		setDescription(GUnitMessages.CompareResultsAction_description);
		setToolTipText(GUnitMessages.CompareResultsAction_tooltip);

		setDisabledImageDescriptor(GUnitPlugin
				.getImageDescriptor("dlcl16/compare.gif")); //$NON-NLS-1$
		setHoverImageDescriptor(GUnitPlugin
				.getImageDescriptor("elcl16/compare.gif")); //$NON-NLS-1$
		setImageDescriptor(GUnitPlugin.getImageDescriptor("elcl16/compare.gif")); //$NON-NLS-1$
		// PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
		// IGUnitHelpContextIds.ENABLEFILTER_ACTION);
		this.fView = view;
	}

	/*
	 * @see Action#actionPerformed
	 */
	@Override
	public void run() {
		final TestElement failedTest = this.fView.getFailedTest();
		if (this.fOpenDialog != null) {
			this.fOpenDialog.setInput(failedTest);
			this.fOpenDialog.getShell().setActive();

		} else {
			this.fOpenDialog = new CompareResultDialog(this.fView.getShell(),
					failedTest);
			this.fOpenDialog.create();
			this.fOpenDialog.getShell().addDisposeListener(
					new DisposeListener() {
						public void widgetDisposed(final DisposeEvent e) {
							CompareResultsAction.this.fOpenDialog = null;
						}
					});
			this.fOpenDialog.setBlockOnOpen(false);
			this.fOpenDialog.open();
		}
	}

	public void updateOpenDialog(final TestElement failedTest) {
		if (this.fOpenDialog != null) {
			this.fOpenDialog.setInput(failedTest);
		}
	}
}
