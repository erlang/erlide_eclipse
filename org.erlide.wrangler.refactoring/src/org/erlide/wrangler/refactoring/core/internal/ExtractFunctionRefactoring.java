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
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Extract function refactoring integration class.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ExtractFunctionRefactoring extends CostumWorkflowRefactoring {

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection selection = GlobalParameters.getWranglerSelection();

        if (!(selection instanceof IErlMemberSelection && (selection.getKind() == SelectionKind.FUNCTION || selection
                .getKind() == SelectionKind.FUNCTION_CLAUSE))) {
            return RefactoringStatus
                    .createFatalErrorStatus("Please select an expression!");
        }

        return new RefactoringStatus();
    }

    @Override
    public String getName() {
        return "Extract function";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "fun_extraction_eclipse", "sxxsi", sel.getFilePath(),
                sel.getSelectionRange().getStartPos(),
                sel.getSelectionRange().getEndPos(), userInput,
                GlobalParameters.getTabWidth());

    }

    @Override
    public RefactoringWorkflowController getWorkflowController(final Shell shell) {
        return new RefactoringWorkflowController(shell) {

            @Override
            public void doRefactoring() {
                final IErlSelection sel = GlobalParameters
                        .getWranglerSelection();
                IRefactoringRpcMessage message = run(sel);
                if (message.isSuccessful()) {
                    changedFiles = message.getRefactoringChangeset();
                    status = new RefactoringStatus();
                } else if (message.getRefactoringState() == RefactoringState.WARNING) {
                    final boolean answer = !ask("Warning",
                            message.getMessageString());
                    if (answer) {
                        message = runAlternative(sel);
                        if (message.getRefactoringState() == RefactoringState.OK) {
                            status = new RefactoringStatus();
                        } else {
                            status = RefactoringStatus
                                    .createFatalErrorStatus(message
                                            .getMessageString());
                        }
                    } else {
                        stop();
                    }
                } else {
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
                "fun_extraction_eclipse", "sxxsi", sel.getFilePath(),
                sel.getSelectionRange().getStartPos(),
                sel.getSelectionRange().getEndPos(), userInput,
                GlobalParameters.getTabWidth());
    }

}
