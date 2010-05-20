package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;

/**
 * Rename function refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RenameFunctionRefactoring extends CostumWorkflowRefactoring {

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		/*
		 * IErlSelection sel = GlobalParameters.getWranglerSelection(); if (sel
		 * instanceof IErlMemberSelection) { SelectionKind kind = sel.getKind();
		 * if (kind == SelectionKind.FUNCTION_CLAUSE || kind ==
		 * SelectionKind.FUNCTION) return new RefactoringStatus(); } return
		 * RefactoringStatus
		 * .createFatalErrorStatus("Please select a function!");
		 */
		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return "Rename function";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		IErlRange memberRange = sel.getSelectionRange();

		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_fun_eclipse", "siisxi", sel.getFilePath(),
				memberRange.getStartLine(), memberRange.getStartCol(),
				userInput, sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	public IRefactoringRpcMessage runAlternative(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		IErlRange memberRange = sel.getMemberRange();

		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_fun_1_eclipse", "siisxi", sel.getFilePath(),
				memberRange.getStartLine(), memberRange.getStartCol(),
				userInput, sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	@Override
	public RefactoringWorkflowController getWorkflowController(Shell shell) {
		return new RefactoringWorkflowController(shell) {

			public void doRefactoring() {
				IErlSelection sel = GlobalParameters.getWranglerSelection();
				IRefactoringRpcMessage message = run(sel);
				if (message.isSuccessful()) {
					changedFiles = message.getRefactoringChangeset();
					status = new RefactoringStatus();
				} else if (message.getRefactoringState() == RefactoringState.WARNING) {
					if (ask("Warning", message.getMessageString())) {
						message = runAlternative(sel);
						if (message.getRefactoringState() == RefactoringState.OK)
							status = new RefactoringStatus();
						else
							status = RefactoringStatus
									.createFatalErrorStatus(message
											.getMessageString());
					} else
						stop();
				} else {
					status = RefactoringStatus.createFatalErrorStatus(message
							.getMessageString());
				}
			}

		};
	}
}
