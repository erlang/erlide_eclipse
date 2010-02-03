package org.erlide.wrangler.refactoring.tmp;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlSelection;

public class FakeRefactoring extends WranglerRefactoring {
	protected String name;
	protected RefactoringStatus finalStatus;

	public FakeRefactoring(String name, RefactoringStatus finalStatus) {
		super();
		this.name = name;
		this.finalStatus = finalStatus;
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		return finalStatus;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection sel) {
		return null;
	}

}
