package org.erlide.wrangler.refactoring.core;

import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.Activator;
import org.erlide.wrangler.refactoring.backend.ChangedFile;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.selection.IErlSelection;

/**
 * Abstract class for implementing Wrangler refactorings. Implementors should
 * extend this.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerRefactoring extends Refactoring {
	protected ArrayList<ChangedFile> changedFiles;

	/**
	 * @return the changed files by the refactoring
	 */
	public ArrayList<ChangedFile> getChangedFiles() {
		return changedFiles;
	}

	/**
	 * Run the RPC call. Usually only one RPC call is needed, for this, this
	 * function is used to do the trick.
	 * 
	 * @param sel
	 *            selected code piece
	 * @return parsed refactoring message
	 */
	public abstract IRefactoringRpcMessage run(IErlSelection sel);

	@Override
	public abstract RefactoringStatus checkFinalConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException;

	@Override
	public abstract RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException;

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {
		pm.beginTask("Creating vhanges", changedFiles.size() + 1);
		CompositeChange change = new CompositeChange(getName());
		pm.internalWorked(1);

		try {
			Change c;
			for (ChangedFile e : changedFiles) {
				c = e.createChanges();
				if (c != null) {
					change.add(c);
					pm.internalWorked(1);
				}
			}
		} catch (IOException e) {
			Status s = new Status(IStatus.ERROR, Activator.PLUGIN_ID, e
					.getMessage());
			throw new CoreException(s);
		}

		return change;
	}

	@Override
	public abstract String getName();

}
