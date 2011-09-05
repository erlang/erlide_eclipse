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
import org.erlide.wrangler.refactoring.selection.IErlSelection.SelectionKind;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

/**
 * Tuple function parameters refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class TupleFunctionParametersRefactoring extends
        SimpleOneStepWranglerRefactoring {
    protected int numberOfTuplingParameters = -1;

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        if (sel instanceof IErlMemberSelection) {
            final SelectionKind kind = sel.getKind();
            if (kind == SelectionKind.FUNCTION_CLAUSE
                    || kind == SelectionKind.FUNCTION) {
                final IErlMemberSelection s = (IErlMemberSelection) sel;
                numberOfTuplingParameters = calculateParametersNumber(WranglerUtils
                        .getTextFromEditor(s.getSelectionRange(),
                                s.getDocument()));
                if (numberOfTuplingParameters > 0) {
                    return new RefactoringStatus();
                }
            }
        }
        return RefactoringStatus
                .createFatalErrorStatus("Please select function parameters!");
    }

    private int calculateParametersNumber(final String textFromEditor) {
        int noC = 0;
        int depth = 0;
        for (int i = 0; i < textFromEditor.length(); ++i) {
            final char c = textFromEditor.charAt(i);
            switch (c) {
            case '{':
            case '(':
            case '[':
                depth++;
                break;
            case ')':
            case '}':
            case ']':
                depth--;
                break;
            case ',':
                if (depth == 0) {
                    noC++;
                }
                break;
            }
        }
        if (depth == 0) {
            return noC + 1;
        } else {
            return -1;
        }
    }

    @Override
    public String getName() {
        return "Tuple functon parameters";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "tuple_funpar_eclipse", "sxxxi", sel.getFilePath(),
                sel.getSelectionRange().getStartPos(),
                sel.getSelectionRange().getEndPos(), sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

}
