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
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Move function refactoring integration class
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class MoveFunctionRefactoring extends SimpleOneStepWranglerRefactoring {

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        /*
         * IErlSelection sel = GlobalParameters.getWranglerSelection(); if (sel
         * instanceof IErlMemberSelection) { SelectionKind kind = sel.getKind();
         * if (kind == SelectionKind.FUNCTION_CLAUSE || kind ==
         * SelectionKind.FUNCTION) return new RefactoringStatus(); } return
         * RefactoringStatus
         * .createFatalErrorStatus("Please select a function!");
         */
        return new RefactoringStatus();
    }

    @Override
    public String getName() {
        return "Move function";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "move_fun_eclipse", "siisxi", sel.getFilePath(),
                sel.getSelectionRange().getStartLine(),
                sel.getSelectionRange().getStartCol(), userInput,
                /* new OtpErlangBoolean(false), */sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

    /**
     * Call an RPC
     * 
     * @param selection
     *            erlang selection
     * @return parsed RPC message
     */
    public IRefactoringRpcMessage run2(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "move_fun_1_eclipse", "siisxxi", sel.getFilePath(),
                sel.getSelectionRange().getStartLine(),
                sel.getSelectionRange().getStartCol(), userInput,
                /* new OtpErlangBoolean(false), */sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

    @Override
    public RefactoringStatus checkFinalConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        IRefactoringRpcMessage message = run(sel);
        if (message.isSuccessful()) {
            changedFiles = message.getRefactoringChangeset();
            return new RefactoringStatus();
        } else if (message.getRefactoringState() == RefactoringState.QUESTION) {
            message = run2(sel);
            if (message.isSuccessful()) {
                changedFiles = message.getRefactoringChangeset();
                return new RefactoringStatus();
            } else {
                return RefactoringStatus.createFatalErrorStatus(message
                        .getMessageString());
            }
        } else {
            return RefactoringStatus.createFatalErrorStatus(message
                    .getMessageString());
        }
    }
}
