package org.erlide.wrangler.refactoring.core;



import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * This kind of Refactoring class is used when a refactoring only needs a single
 * input and does not have any other interactions. e.g. rename variable
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class SimpleOneStepWranglerRefactoring extends
		SimpleWranglerRefactoring {

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		IRefactoringRpcMessage message = run(sel);
		if (message.isSuccessful()) {
			changedFiles = message.getRefactoringChangeset();
			return new RefactoringStatus();
		} else
			// TODO:: warning message could be used, but for this wrangler
			// support is needed
			return RefactoringStatus.createFatalErrorStatus(message
					.getMessage());
	}
}
