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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Shows a dialog with test methods that refer to the selection.
 */
public class GotoReferencedTestAction implements IWorkbenchWindowActionDelegate {
	ISelection fSelection;

	IWorkbenchWindow fWorkbench;

	private void run(final IStructuredSelection selection) {
		final IErlElement[] elements = getSelectedElements(selection);
		if (elements.length == 0) {
			MessageDialog.openInformation(getShell(),
					GUnitMessages.GotoReferencedTestAction_dialog_title,
					GUnitMessages.GotoReferencedTestAction_dialog_message);
			return;
		}
		try {
			run(elements);
		} catch (final CoreException e) {
			ErrorDialog.openError(getShell(),
					GUnitMessages.GotoReferencedTestAction_dialog_title,
					GUnitMessages.GotoReferencedTestAction_dialog_error, e
					.getStatus());
		}
	}

	private void run(final ITextSelection ITextSelection) {
		// try {
		// JavaEditor editor = getActiveEditor();
		// if (editor == null) {
		// return;
		// }
		// IErlElement element = SelectionConverter
		// .getElementAtOffset(editor);
		// int type = element != null ? element.getElementType() : -1;
		// if (type != IErlElement.METHOD && type != IErlElement.TYPE) {
		// element = SelectionConverter.getTypeAtOffset(editor);
		// if (element == null) {
		// MessageDialog
		// .openInformation(
		// getShell(),
		// JUnitMessages.GotoReferencedTestAction_dialog_title,
		// JUnitMessages.GotoReferencedTestAction_dialog_error_nomethod);
		// return;
		// }
		// }
		// run(new IMember[] { (IMember) element });
		// } catch (CoreException e) {
		// ErrorDialog.openError(getShell(),
		// JUnitMessages.GotoReferencedTestAction_dialog_title,
		// JUnitMessages.GotoReferencedTestAction_dialog_error, e
		// .getStatus());
		// }
	}

	private void run(final IErlElement[] elements) throws PartInitException,
	ErlModelException {
		// IErlElement element = elements[0];
		//
		// SelectionStatusDialog dialog = new TestMethodSelectionDialog(
		// getShell(), element);
		// dialog
		// .setTitle(JUnitMessages.GotoReferencedTestAction_selectdialog_title);
		// String msg = Messages.format(
		// JUnitMessages.GotoReferencedTestAction_dialog_select_message,
		// element.getElementName());
		// dialog.setMessage(msg);
		//
		// if (dialog.open() == Window.CANCEL) {
		// return;
		// }
		//
		// Object result = dialog.getFirstResult();
		// if (result == null) {
		// return;
		// }
		//
		// openElement((IErlElement) result);
	}

	private void openElement(final IErlElement result) throws ErlModelException,
	PartInitException {
		// IEditorPart part = JavaUI.openInEditor(result);
		// JavaUI.revealInEditor(part, result);
	}

	private IErlElement[] getSelectedElements(final IStructuredSelection selection) {
		final List<?> elements = selection.toList();
		final int size = elements.size();
		if (size == 0) {
			return new IErlElement[0];
		}

		final ArrayList<IErlElement> result = new ArrayList<IErlElement>(size);

		for (int i = 0; i < size; i++) {
			// Object e = elements.get(i);
			// if (e instanceof ICompilationUnit) {
			// ICompilationUnit unit = (ICompilationUnit) e;
			// IErlModule[] types = new IErlModule[0];
			// try {
			// types = unit.getTypes();
			// } catch (JavaModelException ex) {
			// }
			// for (int j = 0; j < types.length; j++) {
			// result.add(types[j]);
			// }
			// } else if (e instanceof IMethod || e instanceof IErlModule
			// || e instanceof IField) {
			// result.add(e);
			// } else {
			// return new IErlElement[0];
			// }
		}
		return result.toArray(new IErlElement[result.size()]);
	}

	public void run(final IAction action) {
		if (this.fSelection instanceof IStructuredSelection) {
			run((IStructuredSelection) this.fSelection);
		} else if (this.fSelection instanceof ITextSelection) {
			run((ITextSelection) this.fSelection);
		}
	}

	public void selectionChanged(final IAction action, final ISelection selection) {
		this.fSelection = selection;
		action.setEnabled(getActiveEditor() != null);
	}

	private Shell getShell() {
		if (this.fWorkbench != null) {
			return this.fWorkbench.getShell();
		}
		return GUnitPlugin.getActiveWorkbenchShell();
	}

	public void dispose() {
	}

	public void init(final IWorkbenchWindow window) {
		this.fWorkbench = window;
	}

	private ErlangEditor getActiveEditor() {
		final IEditorPart editor = this.fWorkbench.getActivePage().getActiveEditor();
		if (editor instanceof ErlangEditor) {
			return (ErlangEditor) editor;
		}
		return null;
	}
}
