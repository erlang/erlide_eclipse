package org.erlide.wrangler.refactoring.core;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlSelection;

public abstract class CostumWorkflowRefactoring extends
		SimpleWranglerRefactoring {

	public RefactoringStatus status;

	public abstract IRefactoringRpcMessage runAlternative(
			IErlSelection selection);

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		return status;
	}

	public abstract RefactoringWorkflowController getWorkflowController(
			Shell shell);

}
