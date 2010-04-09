package org.erlide.wrangler.refactoring.ui;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldAgainstMacro;
import org.erlide.wrangler.refactoring.core.internal.FoldLocalExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FunctionToProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MoveFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameModuleRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.wizard.DefaultWranglerRefactoringWizard;
import org.erlide.wrangler.refactoring.ui.wizardpages.ComboInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.CostumworkFlowInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SelectionInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SimpleInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

public class PopupMenuAction implements IObjectActionDelegate {

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}

	public void run(IAction action) {
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());

		DefaultWranglerRefactoringWizard wizard = null;
		WranglerRefactoring refactoring = null;
		String actionId = action.getId();

		ArrayList<WranglerPage> pages = new ArrayList<WranglerPage>();
		if (actionId
				.equals("org.erlide.wrangler.refactoring.popupmenu.renamefunction")) {
			pages.add(new CostumworkFlowInputPage("Rename function",
					"Please type the new function name!", "New function name:",
					"New name must be a valid Erlang atom!",
					new AtomValidator()));
			refactoring = new RenameFunctionRefactoring();
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.popupmenu.renamemodule")) {
			pages.add(new CostumworkFlowInputPage("Rename module",
					"Please type the new module name!", "New module name:",
					"New module name must be a valid Erlang atom!",
					new AtomValidator()));
			refactoring = new RenameModuleRefactoring();
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.popupmenu.movefunction")) {

			IProject project = GlobalParameters.getWranglerSelection()
					.getErlElement().getErlProject().getProject();
			ArrayList<String> moduleList = WranglerUtils
					.getModuleNames(project);
			String moduleName = GlobalParameters.getWranglerSelection()
					.getErlElement().getResource().getName();
			moduleList.remove(WranglerUtils.removeExtension(moduleName));

			pages.add(new ComboInputPage("Move function",
					"Please select the destination module",
					"Destination module:", moduleList));
			refactoring = new MoveFunctionRefactoring();
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.popupmenu.foldexpression")) {

			refactoring = new FoldLocalExpressionRefactoring();
			pages
					.add(new SelectionInputPage(
							"Fold expression",
							"Please select expression which should be fold!",
							"Select expressions which should be folded!",
							(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.popupmenu.functiontoprocess")) {
			refactoring = new FunctionToProcessRefactoring();
			pages.add(new SimpleInputPage("Convert function to process",
					"Please type the new process name!", "New process name:",
					"New process name must be an Erlang atom!",
					new AtomValidator()));
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.popupmenu.normailzerecord")) {
			boolean showDefaultFields = MessageDialog.openQuestion(PlatformUI
					.getWorkbench().getDisplay().getActiveShell(),
					"Showing defaults",
					"Show record fields with default values?");
			refactoring = new NormalizeRecordExpression(showDefaultFields);

		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.popumenu.foldagainstmacro")) {
			refactoring = new FoldAgainstMacro();
			pages
					.add(new SelectionInputPage(
							"Fold against macro definition",
							"Please select expression which should be fold!",
							"Select expressions which should be folded!",
							(CostumWorkflowRefactoringWithPositionsSelection) refactoring));
		} else
			return;

		wizard = new DefaultWranglerRefactoringWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE, pages);

		Shell shell = new Shell();
		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
				wizard);

		try {
			op.run(shell, refactoring.getName());
		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	public void selectionChanged(IAction action, ISelection selection) {
	}
}
