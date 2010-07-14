/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
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
	IPath newFile;

	@Override
	public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		// since any selection contains a module, it can be applied
		return new RefactoringStatus();
	}

	@Override
	public String getName() {
		return "Rename module";
	}

	@Override
	public IRefactoringRpcMessage run(final IErlSelection sel) {
		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_mod_eclipse", "ssxi", sel.getFilePath(), userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth());
	}

	@Override
	public Change createChange(final IProgressMonitor pm) throws CoreException,
			OperationCanceledException {

		pm.beginTask("Creating changes", changedFiles.size() + 1);
		CompositeChange change = new CompositeChange(getName());
		pm.internalWorked(1);

		try {
			Change c;
			for (ChangedFile e : changedFiles) {

				if (e.isNameChanged()) {
					IPath p = e.getPath();
					String s = e.getNewName();
					RenameResourceChange rch = new RenameResourceChange(p, s);
					newFile = e.getNewPath();
					// c = e.createChanges((IFile) o);
					// change.add(c);

					change.add(rch);
				} /*
				 * else { c = e.createChanges(); if (c != null) change.add(c); }
				 */
				pm.internalWorked(1);

			}
			/*
			 * } catch (Exception e) { Status s = new Status(IStatus.ERROR,
			 * Activator.PLUGIN_ID, e .getMessage());
			 * 
			 * throw new CoreException(s);
			 */
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			pm.done();
		}

		return change;
	}

	@Override
	public RefactoringWorkflowController getWorkflowController(final Shell shell) {
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
	public IRefactoringRpcMessage runAlternative(final IErlSelection sel) {
		return WranglerBackendManager.getRefactoringBackend().call(
				"rename_mod_1_eclipse", "ssxib", sel.getFilePath(), userInput,
				sel.getSearchPath(), GlobalParameters.getTabWidth(),
				renameTestMod);
	}

	// @Override
	// public void doAfterRefactoring() {
	// WranglerUtils.openFile(WranglerUtils.getFileFromPath(newFile));
	// }
	//
	// @Override
	// public void doBeforeRefactoring() {
	// IEditorPart editorPart = GlobalParameters.getEditor();
	// editorPart.getSite().getPage().closeEditor(editorPart, false);
	// editorPart.dispose();
	// }
}
