package org.erlide.wrangler.refactoring.ui;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.internal.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.internal.ExtractFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldLocalExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldRemoteExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FunctionToProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MoveFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameModuleRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameVariableRefactoring;
import org.erlide.wrangler.refactoring.core.internal.TupleFunctionParametersRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.tmp.CostumworkFlowInputPage;
import org.erlide.wrangler.refactoring.tmp.GeneraliseFunctionRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.validator.VariableNameValidator;
import org.erlide.wrangler.refactoring.ui.wizard.DefaultWranglerRefactoringWizard;
import org.erlide.wrangler.refactoring.ui.wizardpages.ComboInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SelectionInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SimpleInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

public class RefactoringMenuAction extends AbstractWranglerAction {

	public void run(IAction action) {
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());

		DefaultWranglerRefactoringWizard wizard = null;
		WranglerRefactoring refactoring = null;
		String actionId = action.getId();

		ArrayList<WranglerPage> pages = new ArrayList<WranglerPage>();

		// run rename variable refactoring
		if (actionId.equals("org.erlide.wrangler.refactoring.renamevariable")) {
			pages.add(new SimpleInputPage("Rename variable",
					"Please type the new variable name!", "New variable name:",
					"New name must be a valid Erlang variable name!",
					new VariableNameValidator()));
			refactoring = new RenameVariableRefactoring();

			// run rename function refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.renamefunction")) {
			pages.add(new CostumworkFlowInputPage("Rename function",
					"Please type the new function name!", "New function name:",
					"New name must be a valid Erlang atom!",
					new AtomValidator()));
			refactoring = new RenameFunctionRefactoring();

			// run extract function refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.extractfunction")) {
			pages.add(new CostumworkFlowInputPage("Extract function",
					"Please type a function name!", "Function name:",
					"Function name must be a valid Erlang atom!",
					new AtomValidator()));
			refactoring = new ExtractFunctionRefactoring();

			// run rename module refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.renamemodule")) {
			pages.add(new CostumworkFlowInputPage("Rename module",
					"Please type the new module name!", "New module name:",
					"New module name must be a valid Erlang atom!",
					new AtomValidator()));
			refactoring = new RenameModuleRefactoring();

			// run move function refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.movefunction")) {

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

			// run fold expression against a local function
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.foldlocalexpression")) {

			refactoring = new FoldLocalExpressionRefactoring();

			pages
					.add(new SelectionInputPage(
							"Fold expression",
							"Please select expression which should be fold!",
							"Select expressions which should be folded!",
							(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

			// run fold expression against a remote function
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.foldremoteexpression")) {

			// must store the selection, because, the user through the dialog
			// may change it
			IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
					.getWranglerSelection();

			RemoteFunctionClauseDialog dialog = new RemoteFunctionClauseDialog(
					PlatformUI.getWorkbench().getDisplay().getActiveShell(),
					"Fold expression");

			dialog.open();
			dialog.resetSelection();

			if (dialog.isFinished()) {
				IErlFunctionClause functionClause = dialog.getFunctionClause();
				refactoring = new FoldRemoteExpressionRefactoring(
						functionClause, sel);
				pages
						.add(new SelectionInputPage(
								"Fold expression",
								"Please select expression which should be fold!",
								"Select expressions which should be folded!",
								(CostumWorkflowRefactoringWithPositionsSelection) refactoring));
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.introducemacro")) {
				// TODO: start introduce macro refactoring
			} else
				return;

			// run rename process refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.renameprocess")) {
			refactoring = new RenameProcessRefactoring();
			pages.add(new SimpleInputPage("Rename process",
					"Please type the new process name!", "New process name:",
					"New process name must be an Erlang atom!",
					new AtomValidator()));

			// run function to process refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.functiontoprocess")) {
			refactoring = new FunctionToProcessRefactoring();
			pages.add(new SimpleInputPage("Convert function to process",
					"Please type the new process name!", "New process name:",
					"New process name must be an Erlang atom!",
					new AtomValidator()));

			// run tuple function parameters refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.tuplefunctonparameters")) {
			refactoring = new TupleFunctionParametersRefactoring();

			// run generalise function refactoring
		} else if (actionId
				.equals("org.erlide.wrangler.refactoring.generalise")) {
			refactoring = new GeneraliseFunctionRefactoring();
			pages.add(new CostumworkFlowInputPage("Generalise function",
					"Please type the new parameter name!",
					"New parameter name:",
					"New parameter name must be a valid Erlang variable name!",
					new VariableNameValidator()));
			pages
					.add(new SelectionInputPage(
							"Generalise expression",
							"Please select which of them should be generalised!",
							"Select one of them, which should be folded!",
							(CostumWorkflowRefactoringWithPositionsSelection) refactoring));
		} else
			return;

		// run the given refactoring's wizard
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
}
