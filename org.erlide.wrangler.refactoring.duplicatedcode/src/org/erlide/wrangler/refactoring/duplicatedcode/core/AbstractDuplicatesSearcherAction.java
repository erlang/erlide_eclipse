package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.DuplicatesUIManager;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public abstract class AbstractDuplicatesSearcherAction extends AbstractHandler {

	protected static final int TIMEOUT = 180000;

	public Object execute(ExecutionEvent event) throws ExecutionException {
		run();
		return null;
	}

	protected final String rpcErrorMsg = "An error occured during the refactoring!";

	public void run() {
		selectionChanged();
		if (getUserInput()) {
			IProgressMonitor monitor = new NullProgressMonitor();
			try {
				IResultParser result;
				monitor.beginTask("Detecting..", 0);

				result = callRefactoring();
				if (result.isSuccessful()) {
					showDuplicatesView();
					addDuplicates(result.getDuplicates());
				} else {
					DuplicatesUIManager.closeDuplicatesView();
					displayErrorNotification(result.getErrorMessage());
				}
			} catch (WranglerWarningException e) {

			} catch (WranglerRpcParsingException e) {
				displayErrorNotification(rpcErrorMsg);
			} catch (CoreException e) {
				displayErrorNotification(rpcErrorMsg);
			} catch (IOException e) {
				displayErrorNotification(rpcErrorMsg);
			} finally {
				monitor.done();
			}
		}

	}

	protected abstract boolean getUserInput();

	protected void addDuplicates(List<DuplicatedCodeElement> duplicatedCode) {
		DuplicatesUIManager.setRefactoringResults(duplicatedCode);
	}

	protected abstract IResultParser callRefactoring()
			throws WranglerRpcParsingException, CoreException, IOException,
			WranglerWarningException;

	public void selectionChanged() {
		GlobalParameters.setEditor(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getActiveEditor());
	}

	/*
	 * public void setActiveEditor(IAction action, IEditorPart targetEditor) {
	 * GlobalParameters.setEditor(targetEditor); }
	 */

	void displayErrorNotification(String errorMsg) {
		MessageDialog.openError(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getShell(), "Refactoring error",
				errorMsg);

	}

	void showDuplicatesView() {
		DuplicatesUIManager.showDuplicatesView();
	}

}
