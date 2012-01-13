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
package org.erlide.wrangler.refactoring.codeinspection;

import java.util.ArrayList;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.model.erlang.FunctionRef;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.codeinspection.ui.InputDialogWithCheckbox;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Handles commands, which needs only a listing UI, and inspects the code.
 * 
 * @author Gyorgy Orosz
 * 
 */
public class SimpleCodeInspectionHandler extends AbstractHandler implements
        IHandler {
    protected final class IntegerInputValidator implements IInputValidator {
        @Override
        public String isValid(final String newText) {
            try {
                Integer.parseInt(newText);
                return null;
            } catch (final Exception e) {
                return "Please type an integer!";
            }

        }
    }

    static protected final String LARGE_MODULES_VIEW_ID = "largemodules";
    static protected final String DEPENECIES_1_VIEW_ID = "dependencies1";
    static protected final String DEPENECIES_2_VIEW_ID = "dependencies2";
    static protected final String NON_TAIL_RECURSIVE_VIEW_ID = "nontailrecursive";
    static protected final String NOT_FLUSH_UNKNOWN_MESSAGES = "notflush";
    static protected final String NESTED_EXPRESSIONS = "nested";
    static protected final String LONG_FUNCTIONS = "longfunctions";

    @Override
    public Object execute(final ExecutionEvent event) throws ExecutionException {
        final String actionId = event.getCommand().getId();
        PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
                .getActiveEditor().setFocus();
        try {
            GlobalParameters.setSelection(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage().getSelection());
        } catch (final WranglerException e) {
            MessageDialog.openError(PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getShell(), "Error",
                    e.getMessage());
            return null;
        }

        final IErlSelection wranglerSelection = GlobalParameters
                .getWranglerSelection();
        final Shell shell = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getShell();

        if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.largemodules")) {
            handleLargeModulesCall(wranglerSelection, shell);

        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.dependencies")) {
            handleDepenenciesCall(wranglerSelection, shell);
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.nontailrecursive")) {
            handleNonTailRecursiveCall(wranglerSelection, shell);
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.notflush")) {
            handleNotFlushUnknownMessages(wranglerSelection, shell);
        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.nestedif")) {
            handleNested(wranglerSelection, shell, "if");

        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.nestedcase")) {
            handleNested(wranglerSelection, shell, "case");

        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.nestedreceive")) {
            handleNested(wranglerSelection, shell, "receive");

        } else if (actionId
                .equals("org.erlide.wrangler.refactoring.codeinspection.longfunctions")) {
            handleLongFunctions(wranglerSelection, shell);
        }

        return event;
    }

    private void handleLongFunctions(final IErlSelection wranglerSelection,
            final Shell shell) {
        try {
            CodeInspectionViewsManager.hideView(LONG_FUNCTIONS);
            // call inputdialog
            final InputDialogWithCheckbox dialog = new InputDialogWithCheckbox(
                    shell, "Search for long functions", "Number of lines:",
                    "Search in the project", "", new IntegerInputValidator());
            if (Window.OK == dialog.open()) {
                final int linesVal = Integer.parseInt(dialog.getValue());
                final boolean inProject = dialog.isCheckBoxChecked();
                RpcResult res = null;
                if (inProject) {
                    res = WranglerBackendManager.getRefactoringBackend()
                            .callInspection("long_functions_in_dirs_eclipse",
                                    "ixi", linesVal,
                                    wranglerSelection.getSearchPath(),
                                    GlobalParameters.getTabWidth());
                } else {
                    res = WranglerBackendManager.getRefactoringBackend()
                            .callInspection("long_functions_in_file_eclipse",
                                    "sixi", wranglerSelection.getFilePath(),
                                    linesVal,
                                    wranglerSelection.getSearchPath(),
                                    GlobalParameters.getTabWidth());
                }
                // handle rpc
                final ArrayList<IErlElement> elements = processFunctionResult(
                        shell, res);
                if (elements == null) {
                    return;
                }
                // show result
                if (!elements.isEmpty()) {
                    CodeInspectionViewsManager.showErlElements(
                            "Long functions", elements, LONG_FUNCTIONS);
                } else {
                    MessageDialog.openInformation(shell, "No result",
                            "Could not found any function which is longer, than "
                                    + linesVal + " lines.");
                }

            }
        } catch (final Exception e) {
            e.printStackTrace();
        }

    }

    private void handleNested(final IErlSelection wranglerSelection,
            final Shell shell, final String type) {
        try {
            CodeInspectionViewsManager.hideView(NESTED_EXPRESSIONS + type);
            // call inputdialog
            final InputDialogWithCheckbox dialog = new InputDialogWithCheckbox(
                    shell, "Search for nested expression", "Nest level:",
                    "Search in the project", "", new IntegerInputValidator());
            if (Window.OK == dialog.open()) {
                final int nestedVal = Integer.parseInt(dialog.getValue());
                final boolean inProject = dialog.isCheckBoxChecked();
                RpcResult res = null;
                if (inProject) {
                    res = WranglerBackendManager.getRefactoringBackend()
                            .callInspection("nested_exprs_in_dirs_eclipse",
                                    "iaxi", nestedVal, type,
                                    wranglerSelection.getSearchPath(),
                                    GlobalParameters.getTabWidth());
                } else {
                    res = WranglerBackendManager.getRefactoringBackend()
                            .callInspection("nested_exprs_in_file_eclipse",
                                    "siaxi", wranglerSelection.getFilePath(),
                                    nestedVal, type,
                                    wranglerSelection.getSearchPath(),
                                    GlobalParameters.getTabWidth());
                }
                // handle rpc
                final ArrayList<IErlElement> elements = processFunctionResult(
                        shell, res);
                if (elements == null) {
                    return;
                }
                // show result
                if (!elements.isEmpty()) {
                    CodeInspectionViewsManager.showErlElements("Nested " + type
                            + " expressions", elements, NESTED_EXPRESSIONS
                            + type);
                } else {
                    MessageDialog
                            .openInformation(shell, "No result",
                                    "Could not found any " + nestedVal
                                            + " levels nested " + type
                                            + " expression!");
                }

            }
        } catch (final Exception e) {
            e.printStackTrace();
        }

    }

    private void handleNotFlushUnknownMessages(
            final IErlSelection wranglerSelection, final Shell shell) {
        final String inFile = "not_flush_unknown_messages_in_file_eclipse";
        final String inProject = "not_flush_unknown_messages_in_dirs_eclipse";
        CodeInspectionViewsManager.hideView(NOT_FLUSH_UNKNOWN_MESSAGES);
        final Boolean answer = MessageDialog.openQuestion(shell,
                "Find incomplete receive patterns",
                "Would you like to run the scan in the whole project?");
        try {
            RpcResult result = null;
            String function = "";
            if (answer) {
                function = inProject;
                result = WranglerBackendManager.getRefactoringBackend()
                        .callInspection(function, "xi",
                                wranglerSelection.getSearchPath(),
                                GlobalParameters.getTabWidth());
            } else {
                function = inFile;
                result = WranglerBackendManager.getRefactoringBackend()
                        .callInspection(function, "sxi",
                                wranglerSelection.getFilePath(),
                                wranglerSelection.getSearchPath(),
                                GlobalParameters.getTabWidth());
            }

            final ArrayList<IErlElement> elements = processFunctionResult(
                    shell, result);

            if (elements == null) {
                return;
            }
            if (!elements.isEmpty()) {
                CodeInspectionViewsManager.showErlElements(
                        "Incomplete receive patterns", elements,
                        NOT_FLUSH_UNKNOWN_MESSAGES);
            } else {
                MessageDialog.openInformation(shell, "No result",
                        "Could not found any incomplete receive patterns!");
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    private ArrayList<IErlElement> processFunctionResult(final Shell shell,
            final RpcResult result) throws OtpErlangRangeException {
        final ArrayList<IErlElement> elements = new ArrayList<IErlElement>();
        final OtpErlangObject obj = result.getValue();
        final OtpErlangTuple restuple = (OtpErlangTuple) obj;
        final OtpErlangAtom resindicator = (OtpErlangAtom) restuple
                .elementAt(0);
        if (resindicator.atomValue().equals("ok")) {
            final OtpErlangList erlangFunctionList = (OtpErlangList) restuple
                    .elementAt(1);
            for (int i = 0; i < erlangFunctionList.arity(); ++i) {
                final OtpErlangTuple fTuple = (OtpErlangTuple) erlangFunctionList
                        .elementAt(i);
                IErlFunctionClause f;
                try {
                    f = extractFunction(fTuple);
                    elements.add(f);
                } catch (final ErlModelException e) {
                }
            }
        } else {
            final OtpErlangString s = (OtpErlangString) restuple.elementAt(1);
            MessageDialog.openError(shell, "Error", s.stringValue());
            return null;
        }
        return elements;
    }

    private void handleNonTailRecursiveCall(
            final IErlSelection wranglerSelection, final Shell shell) {
        CodeInspectionViewsManager.hideView(NON_TAIL_RECURSIVE_VIEW_ID);

        try {
            final String inFile = "non_tail_recursive_servers_in_file_eclipse";
            final String inProject = "non_tail_recursive_servers_in_dirs_eclipse";
            final Boolean answer = MessageDialog.openQuestion(shell,
                    "Find non tail recursive servers",
                    "Would you like to run the scan in the whole project?");
            String function = "";
            RpcResult res = null;
            if (!answer) {
                function = inFile;
                res = WranglerBackendManager.getRefactoringBackend()
                        .callInspection(function, "sxi",
                                wranglerSelection.getFilePath(),
                                wranglerSelection.getSearchPath(),
                                GlobalParameters.getTabWidth());
            } else {
                function = inProject;
                res = WranglerBackendManager.getRefactoringBackend()
                        .callInspection(function, "xi",
                                wranglerSelection.getSearchPath(),
                                GlobalParameters.getTabWidth());
            }

            final ArrayList<IErlElement> elements = processFunctionResult(
                    shell, res);

            if (elements == null) {
                return;
            }
            if (!elements.isEmpty()) {
                CodeInspectionViewsManager.showErlElements(
                        "Non tail recursive servers", elements,
                        NON_TAIL_RECURSIVE_VIEW_ID);
            } else {
                MessageDialog.openInformation(shell, "No result",
                        "Could not found any non tail recursive server!");
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }

    }

    private IErlFunctionClause extractFunction(final OtpErlangTuple fTuple)
            throws OtpErlangRangeException, ErlModelException {
        final IErlModule mod = extractModule(fTuple.elementAt(0));
        final String function = ((OtpErlangAtom) fTuple.elementAt(1))
                .atomValue();
        final int arity = ((OtpErlangLong) fTuple.elementAt(2)).intValue();
        final IErlFunctionClause f = mod.getModel().findFunction(
                new FunctionRef(mod.getModuleName(), function, arity));
        return f;
    }

    private void handleDepenenciesCall(final IErlSelection wranglerSelection,
            final Shell shell) {
        // hiding the views
        CodeInspectionViewsManager.hideView(
                CodeInspectionViewsManager.CODE_INSPECTION_VIEW,
                DEPENECIES_1_VIEW_ID);
        CodeInspectionViewsManager.hideView(
                CodeInspectionViewsManager.CODE_INSPECTION_VIEW,
                DEPENECIES_2_VIEW_ID);

        // run the rpc
        try {
            final RpcResult res = WranglerBackendManager
                    .getRefactoringBackend().callInspection(
                            "dependencies_of_a_module_eclipse", "sx",
                            wranglerSelection.getFilePath(),
                            wranglerSelection.getSearchPath());

            ArrayList<IErlElement> modules1 = new ArrayList<IErlElement>();
            ArrayList<IErlElement> modules2 = new ArrayList<IErlElement>();

            final OtpErlangObject obj = res.getValue();
            final OtpErlangTuple restuple = (OtpErlangTuple) obj;
            final OtpErlangAtom resindicator = (OtpErlangAtom) restuple
                    .elementAt(0);
            if (resindicator.atomValue().equals("ok")) {
                final OtpErlangTuple listtuple = (OtpErlangTuple) restuple
                        .elementAt(1);
                final OtpErlangList modList1 = (OtpErlangList) listtuple
                        .elementAt(0);
                final OtpErlangList modList2 = (OtpErlangList) listtuple
                        .elementAt(1);
                modules1 = createErlModuleList(modList1);
                modules2 = createErlModuleList(modList2);
            } else {
                final OtpErlangString s = (OtpErlangString) restuple
                        .elementAt(1);
                MessageDialog.openError(shell, "Error", s.stringValue());
                return;
            }

            if (!modules1.isEmpty()) {
                CodeInspectionViewsManager.showErlElements(
                        "Modules which depends on "
                                + wranglerSelection.getErlElement()
                                        .getAncestorOfKind(Kind.MODULE)
                                        .getName(), modules1,
                        DEPENECIES_1_VIEW_ID);
            }
            if (!modules2.isEmpty()) {
                CodeInspectionViewsManager.showErlElements(
                        "Modules, on which "
                                + wranglerSelection.getErlElement()
                                        .getAncestorOfKind(Kind.MODULE)
                                        .getName() + " depends", modules2,
                        DEPENECIES_2_VIEW_ID);
            } else {
                MessageDialog
                        .openInformation(shell, "No result",
                                "There is no large module with the specified parameter!");
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    private ArrayList<IErlElement> createErlModuleList(
            final OtpErlangList modList) {
        final ArrayList<IErlElement> modules = new ArrayList<IErlElement>();
        for (int i = 0; i < modList.arity(); ++i) {
            IErlModule m;
            try {
                m = extractModule(modList.elementAt(i));
                modules.add(m);
            } catch (final ErlModelException e) {
            }
        }
        return modules;
    }

    private IErlModule extractModule(final OtpErlangObject m)
            throws ErlModelException {
        String name = "";
        if (m instanceof OtpErlangString) {
            final OtpErlangString element = (OtpErlangString) m;
            name = element.stringValue();
        } else if (m instanceof OtpErlangAtom) {
            final OtpErlangAtom atom = (OtpErlangAtom) m;
            name = atom.atomValue();
        }
        final String[] modNameParts = name.split("/");
        final IErlModule mod = ErlModelManager.getErlangModel().findModule(
                modNameParts[modNameParts.length - 1]);
        return mod;
    }

    private void handleLargeModulesCall(final IErlSelection wranglerSelection,
            final Shell shell) {
        CodeInspectionViewsManager.hideView(
                CodeInspectionViewsManager.CODE_INSPECTION_VIEW,
                LARGE_MODULES_VIEW_ID);

        final InputDialog dialog = new InputDialog(shell,
                "Lines of a large module", "Lines of a large module:", "",
                new IntegerInputValidator());
        final int ret = dialog.open();
        if (ret == Window.CANCEL) {
            return;
        }
        final int lines = Integer.parseInt(dialog.getValue());
        final RpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callInspection("large_modules_eclipse", "ixi", lines,
                        wranglerSelection.getSearchPath(),
                        GlobalParameters.getTabWidth());

        ArrayList<IErlElement> modules = new ArrayList<IErlElement>();
        try {
            final OtpErlangObject obj = res.getValue();
            final OtpErlangTuple restuple = (OtpErlangTuple) obj;
            final OtpErlangAtom resindicator = (OtpErlangAtom) restuple
                    .elementAt(0);
            if (resindicator.atomValue().equals("ok")) {

                final OtpErlangList modList = (OtpErlangList) restuple
                        .elementAt(1);
                modules = createErlModuleList(modList);
            } else {
                final OtpErlangString s = (OtpErlangString) restuple
                        .elementAt(1);
                MessageDialog.openError(shell, "Error", s.stringValue());
                return;
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }
        if (!modules.isEmpty()) {
            CodeInspectionViewsManager.showErlElements("Large modules",
                    modules, LARGE_MODULES_VIEW_ID);
        } else {
            MessageDialog.openInformation(shell, "No result",
                    "There is no large module with the specified parameter!");
        }
    }
}
