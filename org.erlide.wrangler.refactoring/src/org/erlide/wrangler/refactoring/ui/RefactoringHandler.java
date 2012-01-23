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
package org.erlide.wrangler.refactoring.ui;

import java.util.ArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.utils.ErlUtils;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage;
import org.erlide.wrangler.refactoring.backend.internal.GenFunRefactoringMessage.GenFunReturnParameterName;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.internal.ApplyAdhocElemRefactoring;
import org.erlide.wrangler.refactoring.core.internal.ApplyUserElementaryRefactoring;
import org.erlide.wrangler.refactoring.core.internal.EqcFsmStateDataToRecordRefactoring;
import org.erlide.wrangler.refactoring.core.internal.EqcStatemStateDataToRecordRefactoring;
import org.erlide.wrangler.refactoring.core.internal.ExtractFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldAgainstMacro;
import org.erlide.wrangler.refactoring.core.internal.FoldLocalExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FoldRemoteExpressionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.FunctionToProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.GenFsmStateDataToRecordRefactoring;
import org.erlide.wrangler.refactoring.core.internal.GeneraliseFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.GeneraliseFunctionRefactoring.State;
import org.erlide.wrangler.refactoring.core.internal.IntroduceLetRefactoring;
import org.erlide.wrangler.refactoring.core.internal.IntroduceMacroRefactoring;
import org.erlide.wrangler.refactoring.core.internal.IntroduceNewVariableRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MergeForAllRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MergeLetRefactoring;
import org.erlide.wrangler.refactoring.core.internal.MoveFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.NormalizeRecordExpression;
import org.erlide.wrangler.refactoring.core.internal.PartitionExportsRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameFunctionRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameModuleRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameProcessRefactoring;
import org.erlide.wrangler.refactoring.core.internal.RenameVariableRefactoring;
import org.erlide.wrangler.refactoring.core.internal.TupleFunctionParametersRefactoring;
import org.erlide.wrangler.refactoring.core.internal.UnfoldFunctionApplicationRefactoring;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.ui.validator.AtomValidator;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;
import org.erlide.wrangler.refactoring.ui.validator.ModuleNameValidator;
import org.erlide.wrangler.refactoring.ui.validator.NonEmptyStringValidator;
import org.erlide.wrangler.refactoring.ui.validator.NormalDoulbeValidator;
import org.erlide.wrangler.refactoring.ui.validator.VariableNameValidator;
import org.erlide.wrangler.refactoring.ui.warning.WarningViewManager;
import org.erlide.wrangler.refactoring.ui.wizard.DefaultWranglerRefactoringWizard;
import org.erlide.wrangler.refactoring.ui.wizardpages.ComboInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.CostumworkFlowInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.RecordDataInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SelectionInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.SimpleInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.UserRefacInputPage;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Handles refactoring commands
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RefactoringHandler extends AbstractHandler {

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        try {
            GlobalParameters.setSelection(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage().getSelection());
        } catch (final WranglerException e1) {

            MessageDialog.openError(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), "Error",
                    e1.getMessage());
            return null;
        }

        if (!checkForDirtyEditors()) {
            return null;
        }

        DefaultWranglerRefactoringWizard wizard = null;
        WranglerRefactoring refactoring = null;
        final String actionId = event.getCommand().getId();

        final ArrayList<WranglerPage> pages = new ArrayList<WranglerPage>();

        // apply ad hoc refactoring
        if (actionId.equals("org.erlide.wrangler.refactoring.adhoc")) {
            final InputDialog dialog = getModuleInput(
                    "Apply ad hoc refactoring",
                    "Please type the gen_refac module name!");

            dialog.open();

            if (dialog.getReturnCode() == Window.CANCEL) {
                return null;
            }

            final String callbackModule = dialog.getValue();

            pages.add(new UserRefacInputPage("Apply ad hoc refactoring",
                    "Please type input arguments for this refactoring",
                    "Arguments should not be empty!",
                    new NonEmptyStringValidator()));
            refactoring = new ApplyAdhocElemRefactoring();

            ((ApplyAdhocElemRefactoring) refactoring)
                    .setCallbackModuleName(callbackModule);

            if (!((ApplyAdhocElemRefactoring) refactoring).fetchParPrompts()) {
                MessageDialog.openError(PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell(),
                        "Elementary refactoring error",
                        "Can not load callback module");
                return null;
            }

            // apply user-defined refactoring
        } else if (actionId.equals("org.erlide.wrangler.refactoring.gen_refac")) {
            final String callbackModule = event
                    .getParameter("org.erlide.wrangler.refactoring.gen_refac.callback");
            final String name = event
                    .getParameter("org.erlide.wrangler.refactoring.gen_refac.name");

            pages.add(new UserRefacInputPage(name,
                    "Please type input arguments for this refactoring",
                    "Arguments should not be empty!",
                    new NonEmptyStringValidator()));
            refactoring = new ApplyUserElementaryRefactoring(name,
                    callbackModule);

            if (!((ApplyUserElementaryRefactoring) refactoring)
                    .fetchParPrompts()) {
                MessageDialog.openError(PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getShell(),
                        "Refactoring error", "Can not find callback module");
                return null;
            }

            // run rename variable refactoring
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.renamevariable")) {
            refactoring = new RenameVariableRefactoring();
            final SimpleInputPage page = new SimpleInputPage("Rename variable",
                    "Please type the new variable name!", "New variable name:",
                    "New name must be a valid Erlang variable name!",
                    new VariableNameValidator());
            page.setInput(refactoring.getDefaultValue());
            pages.add(page);

            // introduce new variable refactoring
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.introducenewvariable")) {
            pages.add(new SimpleInputPage("Introduce new variable",
                    "Please type the new variable name!", "New variable name:",
                    "New name must be a valid Erlang variable name!",
                    new VariableNameValidator()));
            refactoring = new IntroduceNewVariableRefactoring();

            // run rename function refactoring
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.renamefunction")) {
            refactoring = new RenameFunctionRefactoring();
            final CostumworkFlowInputPage page = new CostumworkFlowInputPage(
                    "Rename function", "Please type the new function name!",
                    "New function name:",
                    "New name must be a valid Erlang atom!",
                    new AtomValidator());
            page.setInput(refactoring.getDefaultValue());
            pages.add(page);

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
            final boolean answer = MessageDialog
                    .openQuestion(PlatformUI.getWorkbench()
                            .getActiveWorkbenchWindow().getShell(), "Warning!",
                            "The requested operation cannot be undone. Would you like to continue?");

            if (!answer) {
                return null;
            }

            refactoring = new RenameModuleRefactoring();
            final CostumworkFlowInputPage page = new CostumworkFlowInputPage(
                    "Rename module", "Please type the new module name!",
                    "New module name:",
                    "New module name must be a valid Erlang atom!",
                    new AtomValidator());
            page.setInput(refactoring.getDefaultValue());
            pages.add(page);

            // run move function refactoring
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.movefunction")) {

            final IProject project = GlobalParameters.getWranglerSelection()
                    .getErlElement().getProject().getWorkspaceProject();
            final ArrayList<String> moduleList = WranglerUtils
                    .getModuleNames(project);
            final String moduleName = GlobalParameters.getWranglerSelection()
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

            pages.add(new SelectionInputPage(
                    "Fold expression",
                    "Please select expression which should be fold!",
                    "Select expressions which should be folded!",
                    (CostumWorkflowRefactoringWithPositionsSelection) refactoring));

            // run fold expression against a remote function
        } else {
            final Shell activeShell = PlatformUI.getWorkbench().getDisplay()
                    .getActiveShell();
            if (actionId
                    .equals("org.erlide.wrangler.refactoring.foldremoteexpression")) {

                // must store the selection, because, the user through the
                // dialog
                // may change it
                final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                        .getWranglerSelection();

                final RemoteFunctionClauseDialog dialog = new RemoteFunctionClauseDialog(
                        activeShell, "Fold expression");

                dialog.open();
                dialog.resetSelection();

                if (dialog.isFinished()) {
                    final IErlFunctionClause functionClause = dialog
                            .getFunctionClause();
                    refactoring = new FoldRemoteExpressionRefactoring(
                            functionClause, sel);
                    pages.add(new SelectionInputPage(
                            "Fold expression",
                            "Please select expression which should be fold!",
                            "Select expressions which should be folded!",
                            (CostumWorkflowRefactoringWithPositionsSelection) refactoring));

                } else {
                    return null;
                }

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
                } catch (final OtpErlangRangeException e) {
                    e.printStackTrace();
                    return null;
                }

                if (refactoring == null) {
                    return null;
                }

                // fold against macro definition
            } else if (actionId
                    .equals("org.erlide.wrangler.refactoring.foldagainstmacro")) {
                refactoring = new FoldAgainstMacro();

                pages.add(new SelectionInputPage(
                        "Fold against macro definition",
                        "Please select expression which should be fold!",
                        "Select expressions which should be folded!",
                        (CostumWorkflowRefactoringWithPositionsSelection) refactoring));

                // normalize record expression
            } else if (actionId
                    .equals("org.erlide.wrangler.refactoring.normalizerecordexpression")) {
                final boolean showDefaultFields = MessageDialog.openQuestion(
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
                pages.add(new SelectionInputPage(
                        "Merge ?LET expressions",
                        "Please select expressions which whould be merged!",
                        "Select expressions which should be merged",
                        (CostumWorkflowRefactoringWithPositionsSelection) refactoring));
            } else if (actionId
                    .equals("org.erlide.wrangler.refactoring.mergeforall")) {
                refactoring = new MergeForAllRefactoring();
                pages.add(new SelectionInputPage(
                        "Merge ?FORALL expressions",
                        "Please select expressions which should be merged!",
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

            } else if (actionId
                    .equals("org.erlide.wrangler.refactoring.partitionexports")) {
                refactoring = new PartitionExportsRefactoring();
                final SimpleInputPage page = new SimpleInputPage(
                        "Partition exports",
                        "Please input the the distance treshould between 0.1 and 1.0",
                        "Distance treshold",
                        "The value must be between 0.1 and 1.0",
                        new NormalDoulbeValidator());
                page.setInput("0.8");
                pages.add(page);
            } else {
                return null;
            }
        }

        refactoring.doBeforeRefactoring();
        // run the given refactoring's wizard
        wizard = new DefaultWranglerRefactoringWizard(refactoring,
                RefactoringWizard.DIALOG_BASED_USER_INTERFACE, pages);

        final Shell shell = new Shell();
        final RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
                wizard);

        try {
            final int ret = op.run(shell, refactoring.getName());

            if (RefactoringStatus.OK == ret) {
                refactoring.doAfterRefactoring();
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }

        checkWarningMessages();
        return null;

    }

    private InputDialog getModuleInput(final String name, final String mesg) {
        return new InputDialog(PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell(), name, mesg, "",
                new IInputValidator() {

                    public IValidator internalV = new ModuleNameValidator();

                    @Override
                    public String isValid(final String newText) {
                        if (internalV.isValid(newText)) {
                            return null;
                        } else {
                            return "Please type a correct module name!";
                        }
                    }
                });
    }

    private boolean checkForDirtyEditors() {
        final IEditorPart[] dirtyEditors = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage().getDirtyEditors();
        if (dirtyEditors.length > 0) {
            final Boolean answer = MessageDialog
                    .openQuestion(
                            PlatformUI.getWorkbench()
                                    .getActiveWorkbenchWindow().getShell(),
                            "Unsaved changes",
                            "For running Wrangler refactorings, all Erlang files need to be saved. Would you like to continue with saving files?");
            if (answer) {
                for (final IEditorPart ed : dirtyEditors) {
                    if (ed instanceof ITextEditor) {
                        final ITextEditor ted = (ITextEditor) ed;
                        final IFileEditorInput fei = (IFileEditorInput) ted
                                .getEditorInput();
                        if (WranglerUtils.isErlangFile(fei.getFile())) {
                            ed.doSave(null);
                        }

                    }
                }
                return true;
            }
        }
        return true;

    }

    /**
     * Checks whether there is any warning messages, if yes displays a view,
     * containg all of them.
     */
    protected void checkWarningMessages() {
        try {
            final RpcResult res = WranglerBackendManager
                    .getRefactoringBackend().getLoggedInfo();

            if (res.isOk()) {
                final OtpErlangObject resobj = res.getValue();
                if (!resobj.equals(new OtpErlangList())) {
                    final OtpErlangList reslist = (OtpErlangList) resobj;
                    for (int i = 0; i < reslist.arity(); ++i) {
                        final OtpErlangTuple restuple = (OtpErlangTuple) reslist
                                .elementAt(i);
                        final String formattedString = formatWarningString(ErlUtils
                                .asString(restuple.elementAt(1)));
                        WarningViewManager.addWarningMessage(formattedString);
                    }
                }
            } else {
                ErlLogger.error("Wrangler logging error:" + res);
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }

    }

    private String formatWarningString(final String stringValue) {
        try {
            String ret = stringValue.replaceAll("\\s=+\\s", "");
            ret = ret.replaceAll("WARNING:\\s*", "");
            ret = ret.replaceAll("((\\n)(\\n))", "\n");
            ret = ret.replaceAll("\\s+$", "");
            return ret;
        } catch (final Exception e) {
            e.printStackTrace();
            return stringValue;
        }
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
            final ArrayList<WranglerPage> pages, final Shell activeShell)
            throws OtpErlangRangeException {
        WranglerRefactoring refactoring = null;

        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();

        // Ask the user about a new name
        final NewParameterNameInputDialog dialog = new NewParameterNameInputDialog(
                activeShell, "New parameter name");
        dialog.open();
        if (!dialog.isFinished()) {
            return null;
        }

        final String newParamName = dialog.getData();
        dialog.close();

        // call initial RPC
        final GenFunRefactoringMessage m = (GenFunRefactoringMessage) WranglerBackendManager
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

            pages.add(new SelectionInputPage(
                    "Generalise expression",
                    "Please select which of them should be generalised!",
                    "Select one of them, which should be folded!",
                    (CostumWorkflowRefactoringWithPositionsSelection) refactoring));

        } else if (m.getRefactoringState() == RefactoringState.MORE_THAN_ONE_CLAUSE) {
            final boolean selectedClauseOnly = MessageDialog
                    .openQuestion(
                            activeShell,
                            "Multiple clauses",
                            "The function selected has multiple clauses, would you like"
                                    + " to generalise the function clause selected only?");
            refactoring = new GeneraliseFunctionRefactoring(
                    State.more_than_one_clause, m, selectedClauseOnly);
            if (((OtpErlangList) m.getParameters().get(
                    GenFunReturnParameterName.dupsInClause)).arity() > 0) {
                pages.add(new SelectionInputPage(
                        "Generalise expression",
                        "Please select which of them should be generalised!",
                        "Select one of them, which should be folded!",
                        (CostumWorkflowRefactoringWithPositionsSelection) refactoring));
            }

        } else if (m.getRefactoringState() == RefactoringState.UNKNOWN_SIDE_EFFECT) {
            final boolean sideEffect = MessageDialog.openQuestion(activeShell,
                    "Side effect",
                    "Does the expression selected has side effect?");

            final OtpErlangObject noOfClausesPar = m.getParameters().get(
                    GenFunReturnParameterName.noOfClauses);
            if (noOfClausesPar != null
                    && ((OtpErlangLong) noOfClausesPar).intValue() > 1) {
                final boolean selectedClauseOnly = MessageDialog
                        .openQuestion(
                                activeShell,
                                "Multiple clauses",
                                "The function selected has multiple clauses, would you like"
                                        + " to generalise the function clause selected only?");
                refactoring = new GeneraliseFunctionRefactoring(
                        State.unknown_side_effect, m, selectedClauseOnly,
                        sideEffect);

                if (!selectedClauseOnly
                        && ((OtpErlangList) m.getParameters().get(
                                GenFunReturnParameterName.dupsInFun)).arity() > 0
                        || selectedClauseOnly
                        && ((OtpErlangList) m.getParameters().get(
                                GenFunReturnParameterName.dupsInClause))
                                .arity() > 0) {
                    pages.add(new SelectionInputPage(
                            "Generalise expression",
                            "Please select which of them should be generalised!",
                            "Select one of them, which should be folded!",
                            (CostumWorkflowRefactoringWithPositionsSelection) refactoring));
                }

            } else {
                refactoring = new GeneraliseFunctionRefactoring(
                        State.unknown_side_effect, m, true, sideEffect);
                pages.add(new SelectionInputPage(
                        "Generalise expression",
                        "Please select which of them should be generalised!",
                        "Select one of them, which should be folded!",
                        (CostumWorkflowRefactoringWithPositionsSelection) refactoring));

            }

        } else if (m.getRefactoringState() == RefactoringState.ERROR) {
            refactoring = new GeneraliseFunctionRefactoring(State.error,
                    m.getMessageString());

        } else {
            // error?
            return null;
        }
        return refactoring;
    }

}
