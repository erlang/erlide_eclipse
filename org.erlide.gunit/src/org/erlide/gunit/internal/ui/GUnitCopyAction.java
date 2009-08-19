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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.ui.PlatformUI;
import org.erlide.gunit.model.TestElement;

/**
 * Copies a test failure stack trace to the clipboard.
 */
public class GUnitCopyAction extends Action {
	private final FailureTrace fView;

	private final Clipboard fClipboard;

	private TestElement fTestElement;

	/**
	 * Constructor for CopyTraceAction.
	 * 
	 * @param view
	 * @param clipboard
	 */
	public GUnitCopyAction(final FailureTrace view, final Clipboard clipboard) {
		super(GUnitMessages.CopyTrace_action_label);
		Assert.isNotNull(clipboard);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this,
				IGUnitHelpContextIds.COPYTRACE_ACTION);
		this.fView = view;
		this.fClipboard = clipboard;
	}

	/*
	 * @see IAction#run()
	 */
	@Override
	public void run() {
		final String trace = this.fView.getTrace();
		String source = null;
		if (trace != null) {
			source = convertLineTerminators(trace);
		} else if (this.fTestElement != null) {
			source = this.fTestElement.getTestName();
		}
		if (source == null || source.length() == 0) {
			return;
		}

		final TextTransfer plainTextTransfer = TextTransfer.getInstance();
		try {
			this.fClipboard.setContents(
					new String[] { convertLineTerminators(source) },
					new Transfer[] { plainTextTransfer });
		} catch (final SWTError e) {
			if (e.code != DND.ERROR_CANNOT_SET_CLIPBOARD) {
				throw e;
			}
			if (MessageDialog.openQuestion(
					this.fView.getComposite().getShell(),
					GUnitMessages.CopyTraceAction_problem,
					GUnitMessages.CopyTraceAction_clipboard_busy)) {
				run();
			}
		}
	}

	public void handleTestSelected(final TestElement test) {
		this.fTestElement = test;
	}

	private String convertLineTerminators(final String in) {
		final StringWriter stringWriter = new StringWriter();
		final PrintWriter printWriter = new PrintWriter(stringWriter);
		final StringReader stringReader = new StringReader(in);
		final BufferedReader bufferedReader = new BufferedReader(stringReader);
		String line;
		try {
			while ((line = bufferedReader.readLine()) != null) {
				printWriter.println(line);
			}
		} catch (final IOException e) {
			return in; // return the trace unfiltered
		}
		return stringWriter.toString();
	}
}
