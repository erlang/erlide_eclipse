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
import org.eclipse.swt.SWTError;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.PlatformUI;
import org.erlide.gunit.internal.model.TestElement;

/**
 * Copies the names of the methods that failed and their traces to the
 * clipboard.
 */
public class CopyFailureListAction extends Action {

	private final Clipboard fClipboard;

	private final TestRunnerViewPart fRunner;

	public CopyFailureListAction(final TestRunnerViewPart runner, final Clipboard clipboard) {
		super(GUnitMessages.CopyFailureList_action_label);
		this.fRunner = runner;
		this.fClipboard = clipboard;
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IGUnitHelpContextIds.COPYFAILURELIST_ACTION);
	}

	/*
	 * @see IAction#run()
	 */
	@Override
	public void run() {
		final TextTransfer plainTextTransfer = TextTransfer.getInstance();

		try {
			this.fClipboard.setContents(new String[] { getAllFailureTraces() },
					new Transfer[] { plainTextTransfer });
		} catch (final SWTError e) {
			if (e.code != DND.ERROR_CANNOT_SET_CLIPBOARD) {
				throw e;
			}
			// if (MessageDialog.openQuestion(ErlangUIPlugin
			// .getActiveWorkbenchShell(),
			// JUnitMessages.CopyFailureList_problem,
			// JUnitMessages.CopyFailureList_clipboard_busy)) {
			// run();
			// }
		}
	}

	public String getAllFailureTraces() {
		final StringBuffer buf = new StringBuffer();
		final TestElement[] failures = this.fRunner.getAllFailures();

		final String lineDelim = System.getProperty("line.separator", "\n"); //$NON-NLS-1$//$NON-NLS-2$
		for (int i = 0; i < failures.length; i++) {
			final TestElement failure = failures[i];
			buf.append(failure.getTestName()).append(lineDelim);
			final String failureTrace = failure.getTrace();
			if (failureTrace != null) {
				int start = 0;
				while (start < failureTrace.length()) {
					final int idx = failureTrace.indexOf('\n', start);
					if (idx != -1) {
						final String line = failureTrace.substring(start, idx);
						buf.append(line).append(lineDelim);
						start = idx + 1;
					} else {
						start = Integer.MAX_VALUE;
					}
				}
			}
		}
		return buf.toString();
	}

}
