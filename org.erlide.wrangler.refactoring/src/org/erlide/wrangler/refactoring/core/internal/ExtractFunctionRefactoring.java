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
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class ExtractFunctionRefactoring extends CostumWorkflowRefactoring {

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection selection = GlobalParameters.getWranglerSelection();

		if (!((selection instanceof IErlMemberSelection) && (selection
				.getKind() == SelectionKind.FUNCTION || selection.getKind() == SelectionKind.FUNCTION_CLAUSE)))
			return RefactoringStatus
					.createFatalErrorStatus("Please select an expression!");

		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return "Extract function";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"fun_extraction_eclipse", "sxxsi", sel.getFilePath(),
				sel.getSelectionRange().getStartPos(),
				sel.getSelectionRange().getEndPos(), userInput,
				GlobalParameters.getTabWidth());

	}

	@Override
	public RefactoringWorkflowController getWorkflowController(Shell shell) {
		return new RefactoringWorkflowController(shell) {

			@Override
			public void doRefactoring() {
				IErlSelection sel = GlobalParameters.getWranglerSelection();
				IRefactoringRpcMessage message = run(sel);
				if (message.isSuccessful()) {
					changedFiles = message.getRefactoringChangeset();
					status = new RefactoringStatus();
				} else if (message.getRefactoringState() == RefactoringState.WARNING) {
					boolean answer = !ask("Warning", message.getMessageString());
					if (answer) {
						message = runAlternative(sel);
						if (message.getRefactoringState() == RefactoringState.OK) {
							status = new RefactoringStatus();
						} else
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

	@Override
	public IRefactoringRpcMessage runAlternative(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"fun_extraction_eclipse", "sxxsi", sel.getFilePath(),
				sel.getSelectionRange().getStartPos(),
				sel.getSelectionRange().getEndPos(), userInput,
				GlobalParameters.getTabWidth());
	}

}
