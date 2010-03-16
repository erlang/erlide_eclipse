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
 * Integration of the Introduce macro refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class IntroduceMacroRefactoring extends SimpleOneStepWranglerRefactoring {

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		// Guess, no initial condition
		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return "Introduce macro";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection selection) {
		IErlMemberSelection sel = (IErlMemberSelection) selection;
		// TODO: extend it

		return WranglerBackendManager.getRefactoringBackend().call(
				"new_macro_eclipse", "sxxsxi", sel.getFilePath(),
				sel.getSelectionRange().getStartPos(),
				sel.getSelectionRange().getEndPos(), this.userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth());
	}
}
