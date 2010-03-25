package org.erlide.wrangler.refactoring.core.internal;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.ChangedFile;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Rename module refactoring integration class
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RenameModuleRefactoring extends CostumWorkflowRefactoring {

	boolean renameTestMod;

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		// since any selection contains a module, it can be applied
		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return "Rename module";
	}

	@Override
	public IRefactoringRpcMessage run(IErlSelection sel) {
		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_mod_eclipse", "ssxi", sel.getFilePath(), userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {

		CompositeChange c = (CompositeChange) super.createChange(pm);

		for (ChangedFile f : changedFiles) {
			if (f.isNameChanged()) {
				IPath p = f.getIPath();
				String s = f.getNewName();
				RenameResourceChange rch = new RenameResourceChange(p, s);
				c.add(rch);
			}
		}

		return c;
	}

	@Override
	public RefactoringWorkflowController getWorkflowController(Shell shell) {
		return new RefactoringWorkflowController(shell) {

			@Override
			public void doRefactoring() {
				IErlSelection sel = GlobalParameters.getWranglerSelection();
				IRefactoringRpcMessage message = run(sel);
				if (message.isSuccessful()) {
					changedFiles = message.getRefactoringChangeset();
					status = new RefactoringStatus();
				} else if (message.getRefactoringState() == RefactoringState.QUESTION) {
					renameTestMod = ask("Question", message.getMessageString());
					message = runAlternative(sel);
					if (message.getRefactoringState() == RefactoringState.OK) {
						status = new RefactoringStatus();
					} else
						status = RefactoringStatus
								.createFatalErrorStatus(message
										.getMessageString());
				} else if (message.getRefactoringState() == RefactoringState.WARNING) {
					// FIXME: ???
					renameTestMod = !ask("Warning", message.getMessageString());
					if (!renameTestMod) {
						message = runAlternative(sel);
						if (message.getRefactoringState() == RefactoringState.OK) {
							status = new RefactoringStatus();
						} else
							status = RefactoringStatus
									.createFatalErrorStatus(message
											.getMessageString());
					} else
						stop();
				} else {
					status = RefactoringStatus.createFatalErrorStatus(message
							.getMessageString());
				}
			}

		};
	}

	@Override
	public IRefactoringRpcMessage runAlternative(IErlSelection sel) {
		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_mod_1_eclipse", "ssxib", sel.getFilePath(), userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth(),
				renameTestMod);
	}
}
