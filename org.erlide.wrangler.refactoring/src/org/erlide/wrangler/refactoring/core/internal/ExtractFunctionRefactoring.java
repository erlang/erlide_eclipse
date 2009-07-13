package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class ExtractFunctionRefactoring extends
		SimpleOneStepWranglerRefactoring {

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

}
