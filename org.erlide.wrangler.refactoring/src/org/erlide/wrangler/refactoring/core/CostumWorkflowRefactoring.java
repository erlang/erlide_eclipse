package org.erlide.wrangler.refactoring.core;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlSelection;

/**
 * Abstract class for simple Wrangler refactorings, which needs to communicate
 * with the user over the refactoring time.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class CostumWorkflowRefactoring extends
		SimpleWranglerRefactoring {

	protected RefactoringStatus status;

	/**
	 * Runs an RPC like run()
	 * 
	 * @param selection
	 * @return parsed refactoring message
	 */
	public abstract IRefactoringRpcMessage runAlternative(
			IErlSelection selection);

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		return status;
	}

	/**
	 * Returns a worklfow controller object, in which the refactoring behaviour
	 * can be modified.
	 * 
	 * @param shell
	 * @return refactoring workflow controller
	 */
	public abstract RefactoringWorkflowController getWorkflowController(
			Shell shell);

}
