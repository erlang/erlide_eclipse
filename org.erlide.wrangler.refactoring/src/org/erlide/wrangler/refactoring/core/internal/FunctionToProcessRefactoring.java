package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.ProcessRpcMessage;
import org.erlide.wrangler.refactoring.core.ProcessRelatedRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class FunctionToProcessRefactoring extends ProcessRelatedRefactoring {

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		if (sel instanceof IErlMemberSelection) {
			SelectionKind kind = sel.getKind();
			if (kind == SelectionKind.FUNCTION_CLAUSE
					|| kind == SelectionKind.FUNCTION)
				return new RefactoringStatus();
		}
		// TODO: testing
		return RefactoringStatus
				.createFatalErrorStatus("Please select a function!");
	}

	@Override
	protected ProcessRpcMessage checkUndecidables(IErlMemberSelection sel) {
		return (ProcessRpcMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(
						new ProcessRpcMessage(), "fun_to_process_eclipse",
						"siisxi", sel.getFilePath(),
						sel.getSelectionRange().getStartLine(),
						sel.getSelectionRange().getStartCol(), userInput,
						sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	@Override
	protected String getUndecidableWarningMessage() {
		return "There are undecidable cases.";
	}

	@Override
	public String getName() {
		return "Convert function to process";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"fun_to_process_1_eclipse", "siisxi", sel.getFilePath(),
				sel.getMemberRange().getStartLine(),
				sel.getMemberRange().getStartCol(), userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth());
	}
}
