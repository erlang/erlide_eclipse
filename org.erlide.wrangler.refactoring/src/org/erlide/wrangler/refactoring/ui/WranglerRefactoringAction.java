package org.erlide.wrangler.refactoring.ui;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

/**
 * Absrtact refactoring action for Wrangler refactorings.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerRefactoringAction implements
		IWorkbenchWindowActionDelegate, IEditorActionDelegate {

	protected RefactoringParameters parameters = new RefactoringParameters();

	protected WranglerRefactoring refactoring;

	protected WranglerRefactoringWizard refactoringWizard;

	protected String refactoringName;

	public void dispose() {
		refactoringWizard.dispose();
	}


	public void init(IWorkbenchWindow window) {
	}

	/**
	 * Returns with the refactoring"s name.
	 * 
	 * @return the refactoring name
	 */
	protected abstract String initRefactoringName();

	/**
	 * Returns with the specific derivated <code>WranglerRefactoring</code>
	 * 
	 * @return the used refactoring
	 */
	protected abstract WranglerRefactoring initWranglerRefactoring();

	/**
	 * Returns the used refactoring wizard which extends
	 * <code>WranglerRefactoringWizard</code>
	 * 
	 * @return
	 */
	protected abstract WranglerRefactoringWizard initWranglerRefactoringWizard();

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {

		parameters.initSelection();

		refactoringName = initRefactoringName();
		refactoring = initWranglerRefactoring();
		refactoringWizard = initWranglerRefactoringWizard();

		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
				refactoringWizard);

		Shell shell = new Shell();

		try {
			op.run(shell, refactoringName);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		parameters.setSelection(selection);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
	 *      org.eclipse.ui.IEditorPart)
	 */
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		parameters.setEditorPart(targetEditor);
	}

}
