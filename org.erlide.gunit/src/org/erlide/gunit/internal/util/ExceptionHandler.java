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
package org.erlide.gunit.internal.util;

import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * The default exception handler shows an error dialog when one of its handle
 * methods is called. If the passed exception is a <code>CoreException</code> an
 * error dialog pops up showing the exception's status information. For a
 * <code>InvocationTargetException</code> a normal message dialog pops up
 * showing the exception's message. Additionally the exception is written to the
 * platform log.
 * 
 * TO DO: this class is duplicated from org.eclipse.jdt.ui
 */
public class ExceptionHandler {

	private static ExceptionHandler fgInstance = new ExceptionHandler();

	/**
	 * Handles the given <code>CoreException</code>.
	 * 
	 * @param e
	 *            the <code>CoreException</code> to be handled
	 * @param parent
	 *            the dialog window's parent shell
	 * @param title
	 *            the dialog window's window title
	 * @param message
	 *            message to be displayed by the dialog window
	 */
	public static void handle(CoreException e, Shell parent, String title,
			String message) {
		fgInstance.perform(e, parent, title, message);
	}

	/**
	 * Handles the given <code>InvocationTargetException</code>.
	 * 
	 * @param e
	 *            the <code>InvocationTargetException</code> to be handled
	 * @param parent
	 *            the dialog window's parent shell
	 * @param title
	 *            the dialog window's window title
	 * @param message
	 *            message to be displayed by the dialog window
	 */
	public static void handle(InvocationTargetException e, Shell parent,
			String title, String message) {
		fgInstance.perform(e, parent, title, message);
	}

	// ---- Hooks for subclasses to control exception handling
	// ------------------------------------

	protected void perform(CoreException e, Shell shell, String title,
			String message) {
		GUnitPlugin.log(e);
		IStatus status = e.getStatus();
		if (status != null) {
			ErrorDialog.openError(shell, title, message, status);
		} else {
			displayMessageDialog(e, e.getMessage(), shell, title, message);
		}
	}

	protected void perform(InvocationTargetException e, Shell shell,
			String title, String message) {
		Throwable target = e.getTargetException();
		if (target instanceof CoreException) {
			perform((CoreException) target, shell, title, message);
		} else {
			GUnitPlugin.log(e);
			if (e.getMessage() != null && e.getMessage().length() > 0) {
				displayMessageDialog(e, e.getMessage(), shell, title, message);
			} else {
				displayMessageDialog(e, target.getMessage(), shell, title,
						message);
			}
		}
	}

	private void displayMessageDialog(Throwable t, String exceptionMessage,
			Shell shell, String title, String message) {
		StringWriter msg = new StringWriter();
		if (message != null) {
			msg.write(message);
			msg.write("\n\n"); //$NON-NLS-1$
		}
		if (exceptionMessage == null || exceptionMessage.length() == 0)
			msg.write(WizardMessages.ExceptionDialog_seeErrorLogMessage);
		else
			msg.write(exceptionMessage);
		MessageDialog.openError(shell, title, msg.toString());
	}
}
