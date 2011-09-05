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

import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.StateDataToRecordRpcMessage;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangBoolean;

/**
 * Integration of the eqc_statem State Data to record refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class EqcStatemStateDataToRecordRefactoring extends
        QuickCheckStateRefactoring {

    @Override
    public String getName() {
        return "eqc_statem State data to record";
    }

    @Override
    public IRefactoringRpcMessage run(final IErlSelection sel) {

        return WranglerBackendManager.getRefactoringBackend().call(
                "eqc_statem_to_record_1_eclipse", "ssxxxxi", sel.getFilePath(),
                getRecordName(), getFieldsName(), stateFuns,
                new OtpErlangBoolean(fieldCount > 1), sel.getSearchPath(),
                GlobalParameters.getTabWidth());
    }

    @Override
    protected StateDataToRecordRpcMessage runFirst(final IErlMemberSelection sel) {
        final IRefactoringRpcMessage parser = new StateDataToRecordRpcMessage();

        return (StateDataToRecordRpcMessage) WranglerBackendManager
                .getRefactoringBackend().callWithParser(parser,
                        "eqc_statem_to_record_eclipse", "sxi",
                        sel.getFilePath(), sel.getSearchPath(),
                        GlobalParameters.getTabWidth());
    }
}
