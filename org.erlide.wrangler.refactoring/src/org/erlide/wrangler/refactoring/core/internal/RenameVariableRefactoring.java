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

/**
 * Rename variable refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RenameVariableRefactoring extends SimpleOneStepWranglerRefactoring {

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		if (sel instanceof IErlMemberSelection) {
			// SelectionKind kind = sel.getDetailedKind();
			return new RefactoringStatus();
		}

		return RefactoringStatus
				.createFatalErrorStatus("Please select a variable!");
	}

	@Override
	public String getName() {
		return "Rename variable";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_var_eclipse", "siisxi", sel.getFilePath(),
				sel.getSelectionRange().getStartLine(),
				sel.getSelectionRange().getStartCol(), userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

}
