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

/**
 * Rename process refactorings integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RenameProcessRefactoring extends ProcessRelatedRefactoring {

	protected String undecidables;

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
				.createFatalErrorStatus("Please select a process!");
	}

	@Override
	public String getName() {
		return "Rename process";
	}

	@SuppressWarnings("boxing")
	@Override
	public IRefactoringRpcMessage run(IErlSelection sel) {
		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_process_1_eclipse", "sssxi", sel.getFilePath(),
				undecidables, userInput, sel.getSearchPath(),
				GlobalParameters.getTabWidth());
	}

	@SuppressWarnings("boxing")
	@Override
	protected ProcessRpcMessage checkUndecidables(IErlMemberSelection sel) {
		return (ProcessRpcMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(
						new ProcessRpcMessage(), "rename_process_eclipse",
						"siisxi", sel.getFilePath(),
						sel.getSelectionRange().getStartLine(),
						sel.getSelectionRange().getStartCol(), userInput,
						sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	@Override
	protected String getUndecidableWarningMessage() {
		return "Wrangler could not decide whether the new process name provided\n"
				+ "conflicts with the process name(s) used by other"
				+ " registration expression(s).";
	}
}
