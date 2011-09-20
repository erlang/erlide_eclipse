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

import java.util.ArrayList;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.ExpressionPosRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Fold expression against remote function refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class FoldRemoteExpressionRefactoring extends
        CostumWorkflowRefactoringWithPositionsSelection {

    // the selected module, on which, the refactoring will be applied
    private final IErlMemberSelection selection;
    // the selected function clause, which is applied on the module
    private final IErlFunctionClause functionClause;
    protected OtpErlangObject syntaxTree;

    /**
     * Constructor
     * 
     * @param functionClause
     *            selected function clause, which against should be folded
     * @param selection
     *            the current position in the actual file, when the refactoring
     *            was started
     */
    public FoldRemoteExpressionRefactoring(
            final IErlFunctionClause functionClause,
            final IErlMemberSelection selection) {
        this.functionClause = functionClause;
        this.selection = selection;
    }

    @Override
    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return null;
    }

    @Override
    public IRefactoringRpcMessage runAlternative(
            final IErlSelection theSelection) {
        return null;
    }

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        if (functionClause == null) {
            return RefactoringStatus
                    .createFatalErrorStatus("No function clause was given!");
        } else {

            ExpressionPosRpcMessage m = new ExpressionPosRpcMessage();
            final String path = selection.getFilePath();
            final String moduleName = functionClause.getModule()
                    .getModuleName();
            final String functionName = functionClause.getFunctionName();
            final int arity = functionClause.getArity();

            int clauseIndex = 1;
            if (!(functionClause instanceof IErlFunction)) {
                // FIXME: avoid hacking!!!
                clauseIndex = Integer.valueOf(functionClause.getName()
                        .substring(1));
            }

            m = (ExpressionPosRpcMessage) WranglerBackendManager
                    .getRefactoringBackend().callWithParser(m,
                            "fold_expr_by_name_eclipse", "sssiixi", path,
                            moduleName, functionName, arity, clauseIndex,
                            selection.getSearchPath(),
                            GlobalParameters.getTabWidth());

            if (m.isSuccessful()) {
                syntaxTree = m.getSyntaxTree();
                // TODO: store positions, selectedpositions
                positions = m.getPositionDefinitions(selection.getDocument());
                selectedPositions = new ArrayList<IErlRange>();
            } else {
                return RefactoringStatus.createFatalErrorStatus(m
                        .getMessageString());
            }
        }

        return new RefactoringStatus();
    }

    @Override
    public String getName() {
        return "Fold expression";
    }

    @Override
    public RefactoringStatus checkFinalConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        final IRefactoringRpcMessage message = run(sel);
        if (message.isSuccessful()) {
            changedFiles = message.getRefactoringChangeset();
            return new RefactoringStatus();
        } else {
            return RefactoringStatus.createFatalErrorStatus(message
                    .getMessageString());
        }
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection theSelection) {
        final IErlMemberSelection sel = (IErlMemberSelection) theSelection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "fold_expr_1_eclipse", "sxxxi", sel.getFilePath(), syntaxTree,
                getSelectedPos(), sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

}
