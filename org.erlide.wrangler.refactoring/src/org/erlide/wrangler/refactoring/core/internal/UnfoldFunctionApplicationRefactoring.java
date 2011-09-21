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
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

/**
 * Unfold tuple application refactoring integration class.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class UnfoldFunctionApplicationRefactoring extends
        SimpleOneStepWranglerRefactoring {

    /**
     * Default constructor
     */
    public UnfoldFunctionApplicationRefactoring() {
    }

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        if (sel instanceof IErlMemberSelection) {
            /*
             * if (sel.getKind() == SelectionKind.FUNCTION || sel.getKind() ==
             * SelectionKind.FUNCTION_CLAUSE)
             */
            return new RefactoringStatus();
        }

        return RefactoringStatus
                .createFatalErrorStatus("Please select a function!");
    }

    @Override
    public String getName() {
        return "Unfold Function Application";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "unfold_fun_app_eclipse", "sxxi", sel.getFilePath(),
                sel.getSelectionRange().getStartPos(), sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

}
