package org.erlide.wrangler.refactoring.ui;

import java.util.ArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.IErlFunctionClause;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage.GenFunReturnParameterName;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.internal.EqcFsmStateDataToRecordRefactoring;
import org.erlide.wrangler.refactoring.core.internal.EqcStatemStateDataToRecordRefactoring;
import org.erlide.wrangler.refactoring.core.internal.ExtractFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldAgainstMacro;
import org.erlide.wrangler.refactoring.core.internal.FoldLocalExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldRemoteExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FunctionToProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.GenFsmStateDataToRecordRefactoring;
import org.erlide.wrangler.refactoring.core.internal.GeneraliseFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.IntroduceLetRefactoring;
import org.erlide.wrangler.refactoring.core.internal.IntroduceMacroRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MergeForAllRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MergeLetRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MoveFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameModuleRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameVariableRefactoring;
import org.erlide.wrangler.refactoring.core.internal.TupleFunctionParametersRefactoring;
import org.erlide.wrangler.refactoring.core.internal.UnfoldFunctionApplicationRefactoring;
import org.erlide.wrangler.refactoring.core.internal.GeneraliseFunctionRefactoring.State;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.validator.NonEmptyStringValidator;
import org.erlide.wrangler.refactoring.ui.validator.VariableNameValidator;
import org.erlide.wrangler.refactoring.ui.wizard.DefaultWranglerRefactoringWizard;
import org.erlide.wrangler.refactoring.ui.wizardpages.ComboInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.CostumworkFlowInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.RecordDataInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SelectionInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SimpleInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public class RefactoringHandler extends AbstractHandler {

	public Object execute(ExecutionEvent event) throws ExecutionException {
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());

		DefaultWranglerRefactoringWizard wizard = null;
		WranglerRefactoring refactoring = null;
		String actionId = event.getCommand().getId();

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
		} else {
			Shell activeShell = PlatformUI.getWorkbench().getDisplay()
					.getActiveShell();
			if (actionId
					.equals("org.erlide.wrangler.refactoring.foldremoteexpression")) {

				// must store the selection, because, the user through the
				// dialog
				// may change it
				IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
						.getWranglerSelection();

				RemoteFunctionClauseDialog dialog = new RemoteFunctionClauseDialog(
						activeShell, "Fold expression");

				dialog.open();
				dialog.resetSelection();

				if (dialog.isFinished()) {
					IErlFunctionClause functionClause = dialog
							.getFunctionClause();
					refactoring = new FoldRemoteExpressionRefactoring(
							functionClause, sel);
					pages
							.add(new SelectionInputPage(
									"Fold expression",
									"Please select expression which should be fold!",
									"Select expressions which should be folded!",
									(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

				} else
					return null;

				// run introduce macro refactoring
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.introducemacro")) {
				refactoring = new IntroduceMacroRefactoring();
				pages.add(new SimpleInputPage("Introduce macro definition",
						"Please type the new macro name!", "New macro name:",
						"Macro name cannot be empty!",
						new NonEmptyStringValidator()));
				// run rename process refactoring
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.renameprocess")) {
				refactoring = new RenameProcessRefactoring();
				pages.add(new SimpleInputPage("Rename process",
						"Please type the new process name!",
						"New process name:",
						"New process name must be an Erlang atom!",
						new AtomValidator()));

				// run function to process refactoring
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.functiontoprocess")) {
				refactoring = new FunctionToProcessRefactoring();
				pages.add(new SimpleInputPage("Convert function to process",
						"Please type the new process name!",
						"New process name:",
						"New process name must be an Erlang atom!",
						new AtomValidator()));

				// run tuple function parameters refactoring
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.tuplefunctonparameters")) {
				refactoring = new TupleFunctionParametersRefactoring();

				// run generalise function refactoring
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.generalise")) {
				/*
				 * pages.add(new CostumworkFlowInputPage("Generalise function",
				 * "Please type the new parameter name!", "New parameter name:",
				 * "New parameter name must be a valid Erlang variable name!",
				 * new VariableNameValidator()));
				 */
				try {
					refactoring = runGenFunRefactoring(pages, activeShell);
				} catch (OtpErlangRangeException e) {
					e.printStackTrace();
					return null;
				}

				if (refactoring == null)
					return null;

				// fold against macro definition
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.foldagainstmacro")) {
				refactoring = new FoldAgainstMacro();

				pages
						.add(new SelectionInputPage(
								"Fold against macro definition",
								"Please select expression which should be fold!",
								"Select expressions which should be folded!",
								(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

				// normalize record expression
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.normalizerecordexpression")) {
				boolean showDefaultFields = MessageDialog.openQuestion(
						activeShell, "Showing defaults",
						"Show record fields with default values?");
				refactoring = new NormalizeRecordExpression(showDefaultFields);
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.introducelet")) {

				pages.add(new CostumworkFlowInputPage("Introduce ?LET",
						"Please type the pattern variable name!",
						"Pattern variable name:",
						"New name must be a valid Erlang atom!",
						new VariableNameValidator()));
				refactoring = new IntroduceLetRefactoring();
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.mergelet")) {
				refactoring = new MergeLetRefactoring();
				pages
						.add(new SelectionInputPage(
								"Merge ?LET expressions",
								"Please select expressions which whould be merged!",
								"Select expressions which should be merged",
								(CostumWorkflowRefactoringWithPositionsSelection) refactoring));
			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.mergeforall")) {
				refactoring = new MergeForAllRefactoring();
				pages
						.add(new SelectionInputPage(
								"Merge ?FORALL expressions",
								"Please select expressions which whould be merged!",
								"Select expressions which should be merged",
								(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.eqc_statemtorecord")) {
				refactoring = new EqcStatemStateDataToRecordRefactoring();
				pages.add(new RecordDataInputPage(
						"eqc_statem State Data To Record"));

			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.eqc_fsmtorecord")) {
				refactoring = new EqcFsmStateDataToRecordRefactoring();
				pages.add(new RecordDataInputPage(
						"eqc_fsm State Data To Record"));

			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.gen_fsmtorecord")) {
				refactoring = new GenFsmStateDataToRecordRefactoring();
				pages.add(new RecordDataInputPage(
						"gen_fsm State Data To Record"));

			} else if (actionId
					.equals("org.erlide.wrangler.refactoring.unfoldfunctionapplication")) {
				refactoring = new UnfoldFunctionApplicationRefactoring();
			}

			else
				return null;
		}

		// run the given refactoring's wizard
		wizard = new DefaultWranglerRefactoringWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE, pages);

		Shell shell = new Shell();
		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
				wizard);

		try {
			int ret = op.run(shell, refactoring.getName());
			if (RefactoringStatus.OK == ret)
				WranglerUtils.notifyErlide(refactoring.getChangedFiles());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;

	}

	/**
	 * Run the generalise function refactoring
	 * 
	 * @param pages
	 * @param activeShell
	 * @return
	 * @throws OtpErlangRangeException
	 */
	protected WranglerRefactoring runGenFunRefactoring(
			ArrayList<WranglerPage> pages, Shell activeShell)
			throws OtpErlangRangeException {
		WranglerRefactoring refactoring = null;

		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();

		// Ask the user about a new name
		NewParameterNameInputDialog dialog = new NewParameterNameInputDialog(
				activeShell, "New parameter name");
		dialog.open();
		if (!dialog.isFinished())
			return null;

		String newParamName = dialog.getData();
		dialog.close();

		// call initial RPC
		GenFunRefactoringMessage m = (GenFunRefactoringMessage) WranglerBackendManager
				.getRefactoringBackend().callWithParser(
						new GenFunRefactoringMessage(), "generalise_eclipse",
						"sxxsxi", sel.getFilePath(),
						sel.getSelectionRange().getStartPos(),
						sel.getSelectionRange().getEndPos(), newParamName,
						sel.getSearchPath(), GlobalParameters.getTabWidth());

		// Examine the result of the refactoring: 4 cases
		if (m.getRefactoringState() == RefactoringState.OK) {
			refactoring = new GeneraliseFunctionRefactoring(State.ok, m);

		} else if (m.getRefactoringState() == RefactoringState.MULTI_INSTANCES) {

			refactoring = new GeneraliseFunctionRefactoring(
					State.multi_instance, m);

			pages
					.add(new SelectionInputPage(
							"Generalise expression",
							"Please select which of them should be generalised!",
							"Select one of them, which should be folded!",
							(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

		} else if (m.getRefactoringState() == RefactoringState.MORE_THAN_ONE_CLAUSE) {
			boolean selectedClauseOnly = MessageDialog
					.openQuestion(
							activeShell,
							"Multiple clauses",
							"The function selected has multiple clauses, would you like"
									+ " to generalise the function clause selected only?");
			refactoring = new GeneraliseFunctionRefactoring(
					State.more_than_one_clause, m, selectedClauseOnly);
			if (((OtpErlangList) m.getParameters().get(
					GenFunReturnParameterName.dupsInClause)).arity() > 0)
				pages
						.add(new SelectionInputPage(
								"Generalise expression",
								"Please select which of them should be generalised!",
								"Select one of them, which should be folded!",
								(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

		} else if (m.getRefactoringState() == RefactoringState.UNKNOWN_SIDE_EFFECT) {
			boolean sideEffect = MessageDialog.openQuestion(activeShell,
					"Side effect",
					"Does the expression selected has side effect?");

			OtpErlangObject noOfClausesPar = m.getParameters().get(
					GenFunReturnParameterName.noOfClauses);
			if (noOfClausesPar != null
					&& ((OtpErlangLong) noOfClausesPar).intValue() > 1) {
				boolean selectedClauseOnly = MessageDialog
						.openQuestion(
								activeShell,
								"Multiple clauses",
								"The function selected has multiple clauses, would you like"
										+ " to generalise the function clause selected only?");
				refactoring = new GeneraliseFunctionRefactoring(
						State.unknown_side_effect, m, selectedClauseOnly,
						sideEffect);

				if ((!selectedClauseOnly && ((OtpErlangList) m.getParameters()
						.get(GenFunReturnParameterName.dupsInFun)).arity() > 0)
						|| (selectedClauseOnly && ((OtpErlangList) m
								.getParameters().get(
										GenFunReturnParameterName.dupsInClause))
								.arity() > 0))
					pages
							.add(new SelectionInputPage(
									"Generalise expression",
									"Please select which of them should be generalised!",
									"Select one of them, which should be folded!",
									(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

			} else {
				refactoring = new GeneraliseFunctionRefactoring(
						State.unknown_side_effect, m, true, sideEffect);
				pages
						.add(new SelectionInputPage(
								"Generalise expression",
								"Please select which of them should be generalised!",
								"Select one of them, which should be folded!",
								(CostumWorkflowRefactoringWithPositionsSelection) refactoring));

			}

		} else if (m.getRefactoringState() == RefactoringState.ERROR) {
			refactoring = new GeneraliseFunctionRefactoring(State.error, m
					.getMessageString());

		} else
			// error?
			return null;
		return refactoring;
	}

}
