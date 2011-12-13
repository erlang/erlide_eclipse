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
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Rename variable refactoring integration
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RenameVariableRefactoring extends SimpleOneStepWranglerRefactoring {

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        final IErlSelection sel = GlobalParameters.getWranglerSelection();
        if (sel instanceof IErlMemberSelection) {
            // SelectionKind kind = sel.getDetailedKind();
            return new RefactoringStatus();
        }

        return RefactoringStatus
                .createFatalErrorStatus("Please select a variable!");
    }

    @Override
    public String getName() {
        return "Rename variable";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection selection) {
        final IErlMemberSelection sel = (IErlMemberSelection) selection;
        return WranglerBackendManager.getRefactoringBackend().call(
                "rename_var_eclipse", "siisxi", sel.getFilePath(),
                sel.getSelectionRange().getStartLine(),
                sel.getSelectionRange().getStartCol(), userInput,
                sel.getSearchPath(), GlobalParameters.getTabWidth());
    }

    @Override
    public String getDefaultValue() {
        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();
        if (sel == null) {
            return "";
        }

        final RpcResult res = WranglerBackendManager.getRefactoringBackend()
                .callWithoutParser("get_var_name_eclipse", "siixi",
                        sel.getFilePath(),
                        sel.getSelectionRange().getStartLine(),
                        sel.getSelectionRange().getStartCol(),
                        sel.getSearchPath(), GlobalParameters.getTabWidth());

        if (res.getValue().getClass().equals(OtpErlangString.class)) {
            return ((OtpErlangString) res.getValue()).stringValue();
        } else {
            return "";
        }
    }

}
