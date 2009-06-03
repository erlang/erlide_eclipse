package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.DuplicatesUIManager;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;

public abstract class AbstractDuplicatesSearcherAction implements
		IEditorActionDelegate {

	protected final String rpcErrorMsg = "An error occured during the refactoring!";
	protected RefactoringParameters parameter = new RefactoringParameters();

	public void run(IAction action) {
		if (getUserInput()) {
			try {
				IResultParser result;

				result = callRefactoring();
				if (result.isSuccessful()) {
					showDuplicatesView();
					addDuplicates(result.getDuplicates());
				} else {
					displayErrorNotification(result.getErrorMessage());
				}
			} catch (WranglerWarningException e) {

			} catch (BackendException e) {
				displayErrorNotification(rpcErrorMsg);
			} catch (CoreException e) {
				displayErrorNotification(rpcErrorMsg);
			} catch (IOException e) {
				displayErrorNotification(rpcErrorMsg);
			}
		}

	}

	protected abstract boolean getUserInput();

	protected void addDuplicates(List<DuplicatedCodeElement> duplicatedCode) {
		DuplicatesUIManager.setRefactoringResults(duplicatedCode);
	}

	protected abstract IResultParser callRefactoring() throws BackendException,
			CoreException, IOException, WranglerWarningException;

	public void selectionChanged(IAction action, ISelection selection) {
		parameter.setSelection(selection);
	}

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		parameter.setEditorPart(targetEditor);
	}

	void displayErrorNotification(String errorMsg) {
		MessageDialog.openError(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getShell(), "Refactoring error",
				errorMsg);

	}

	void showDuplicatesView() {
		DuplicatesUIManager.showDuplicatesView();
	}

}
