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
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.ExpressionPosRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Fold expression against local function refactoring integrations
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class FoldLocalExpressionRefactoring extends
        CostumWorkflowRefactoringWithPositionsSelection {

    protected OtpErlangObject syntaxTree;

    /**
     * Preprocessing the file(s), and finding the candidates to fold.
     */
    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {

        final IErlSelection selection = GlobalParameters.getWranglerSelection();

        if (!(selection instanceof IErlMemberSelection && (selection.getKind() == SelectionKind.FUNCTION || selection
                .getKind() == SelectionKind.FUNCTION_CLAUSE))) {
            return RefactoringStatus
                    .createFatalErrorStatus("Please select an expression!");
        }

        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        ExpressionPosRpcMessage m = new ExpressionPosRpcMessage();
        m = (ExpressionPosRpcMessage) WranglerBackendManager
                .getRefactoringBackend().callWithParser(m,
                        "fold_expr_by_loc_eclipse", "siixi", sel.getFilePath(),
                        sel.getMemberRange().getStartLine(),
                        sel.getMemberRange().getStartCol(),
                        sel.getSearchPath(), GlobalParameters.getTabWidth());
        if (m.isSuccessful()) {
            syntaxTree = m.getSyntaxTree();
            positions = m.getPositionDefinitions(sel.getDocument());
            selectedPositions = new ArrayList<IErlRange>();
        } else {
            return RefactoringStatus.createFatalErrorStatus(m
                    .getMessageString());
        }
        return new RefactoringStatus();

    }

    @Override
    public String getName() {
        return "Fold expression";
    }

    /**
     * Fold the selected expression(s).
     */
    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "fold_expr_1_eclipse", "sxxxi", sel.getFilePath(), syntaxTree,
                getSelectedPos(), sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

    @Override
    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return new RefactoringWorkflowController(shell) {
            @Override
            public void doRefactoring() {
            }
        };
    }

    @Override
    public IRefactoringRpcMessage runAlternative(final IErlSelection selection) {
        return null;
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

}
