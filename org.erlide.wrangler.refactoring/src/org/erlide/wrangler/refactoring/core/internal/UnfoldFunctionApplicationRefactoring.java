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
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class UnfoldFunctionApplicationRefactoring extends
		SimpleOneStepWranglerRefactoring {

	public UnfoldFunctionApplicationRefactoring() {
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		if (sel instanceof IErlMemberSelection) {
			/*
			 * if (sel.getKind() == SelectionKind.FUNCTION || sel.getKind() ==
			 * SelectionKind.FUNCTION_CLAUSE)
			 */
			return new RefactoringStatus();
		}

		return RefactoringStatus
				.createFatalErrorStatus("Please select a function!");
	}

	@Override
	public String getName() {
		return "Unfold Function Application";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"unfold_fun_app_eclipse", "sxxi", sel.getFilePath(),
				sel.getSelectionRange().getStartPos(), sel.getSearchPath(),
				GlobalParameters.getTabWidth());
	}

}
