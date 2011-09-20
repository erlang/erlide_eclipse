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
import org.erlide.wrangler.refactoring.backend.internal.ProcessRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Function to process refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class FunctionToProcessRefactoring extends ProcessRelatedRefactoring {

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        if (sel instanceof IErlMemberSelection) {
            final SelectionKind kind = sel.getKind();
            if (kind == SelectionKind.FUNCTION_CLAUSE
                    || kind == SelectionKind.FUNCTION) {
                return new RefactoringStatus();
            }
        }
        // TODO: testing
        return RefactoringStatus
                .createFatalErrorStatus("Please select a function!");
    }

    @Override
    protected ProcessRpcMessage checkUndecidables(final IErlMemberSelection sel) {
        return (ProcessRpcMessage) WranglerBackendManager
                .getRefactoringBackend().callWithParser(
                        new ProcessRpcMessage(), "fun_to_process_eclipse",
                        "siisxi", sel.getFilePath(),
                        sel.getSelectionRange().getStartLine(),
                        sel.getSelectionRange().getStartCol(), userInput,
                        sel.getSearchPath(), GlobalParameters.getTabWidth());
    }

    @Override
    protected String getUndecidableWarningMessage() {
        return "There are undecidable cases.";
    }

    @Override
    public String getName() {
        return "Convert function to process";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "fun_to_process_1_eclipse", "siisxi", sel.getFilePath(),
                sel.getMemberRange().getStartLine(),
                sel.getMemberRange().getStartCol(), userInput,
                sel.getSearchPath(), GlobalParameters.getTabWidth());
    }
}
