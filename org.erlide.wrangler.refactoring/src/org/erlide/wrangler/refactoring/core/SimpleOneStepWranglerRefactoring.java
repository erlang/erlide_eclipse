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

	/**
	 * If the refactoring returns with a warning message, wrangler should know
	 * that the user asked to continue. It is done by calling a function.
	 * 
	 * @param sel
	 * @return a refactoring message
	 */
	// public abstract IRefactoringRpcMessage runAfterWarning(IErlSelection
	// sel);
	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		IErlSelection sel = GlobalParameters.getWranglerSelection();
		IRefactoringRpcMessage message = run(sel);
		if (message.isSuccessful()) {
			changedFiles = message.getRefactoringChangeset();
			return new RefactoringStatus();
			// } else if (message.getRefactoringState() ==
			// RefactoringState.WARNING) {
			// return RefactoringStatus.createWarningStatus(message
			// .getMessageString());
		} else {
			return RefactoringStatus.createFatalErrorStatus(message
					.getMessageString());
		}
	}
}
