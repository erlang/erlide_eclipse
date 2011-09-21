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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.internal.RefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Introduce ?LET expression refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class IntroduceLetRefactoring extends CostumWorkflowRefactoring {

    OtpErlangObject expr, parentExpr;

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        // FIXME: what are the preconditions? add them!
        return new RefactoringStatus();
    }

    @Override
    public String getName() {
        return "Introduce ?LET";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "new_let_eclipse", "sxxsxi", sel.getFilePath(),
                sel.getSelectionRange().getStartPos(),
                sel.getSelectionRange().getEndPos(), userInput,
                sel.getSearchPath(), GlobalParameters.getTabWidth());
    }

    @Override
    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return new RefactoringWorkflowController(shell) {

            @Override
            public void doRefactoring() {
                final IErlSelection sel = GlobalParameters
                        .getWranglerSelection();
                RefactoringRpcMessage message = (RefactoringRpcMessage) run(sel);
                if (message.isSuccessful()) {
                    changedFiles = message.getRefactoringChangeset();
                    status = new RefactoringStatus();
                } else if (message.getRefactoringState() == RefactoringState.QUESTION) {
                    if (ask("Question", message.getMessageString())) {
                        final OtpErlangTuple res = message.getResultObject();
                        expr = ((OtpErlangTuple) res.elementAt(2)).elementAt(0);
                        parentExpr = ((OtpErlangTuple) res.elementAt(2))
                                .elementAt(1);
                        message = (RefactoringRpcMessage) runAlternative(sel);
                        if (message.isSuccessful()) {
                            status = new RefactoringStatus();
                            changedFiles = message.getRefactoringChangeset();
                        } else {
                            status = RefactoringStatus
                                    .createFatalErrorStatus(message
                                            .getMessageString());
                        }
                    } else {
                        status = RefactoringStatus
                                .createFatalErrorStatus("Refactoring failed: the expression selected is not a QuickCheck generator.");
                    }

                } else if (message.getRefactoringState() == RefactoringState.ERROR) {
                    status = RefactoringStatus.createFatalErrorStatus(message
                            .getMessageString());
                }

            }

        };
    }

    @Override
    public IRefactoringRpcMessage runAlternative(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "new_let_1_eclipse", "ssxxxi", sel.getFilePath(), userInput,
                expr, parentExpr, sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

}
