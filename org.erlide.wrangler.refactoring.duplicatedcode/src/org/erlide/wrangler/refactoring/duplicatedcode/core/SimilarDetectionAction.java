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
package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.internal.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.internal.WranglerRefactoringBackend;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.SimilarCodeDetectionInputDialog;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Similar code detection refactoring runner
 * 
 * @author Gyorgy Orosz
 * 
 */
public class SimilarDetectionAction extends AbstractDuplicatesSearcherAction {

    int minLen;
    int minFreq;
    int minToks;
    int maxNewVars;
    double simScore;
    boolean onlyInFile;

    @Override
    protected IResultParser callRefactoring()
            throws WranglerRpcParsingException, CoreException, IOException,
            WranglerWarningException {
        final WranglerRefactoringBackend backend = WranglerBackendManager
                .getRefactoringBackend();
        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();

        RpcResult result;
        final String functionName = "sim_code_detection_eclipse";
        if (onlyInFile) {

            final OtpErlangString fp = new OtpErlangString(sel.getFilePath());
            final OtpErlangString[] fpa = new OtpErlangString[1];
            fpa[0] = fp;
            final OtpErlangList fpl = new OtpErlangList(fpa);

            result = backend.callWithoutParser(
                    WranglerRefactoringBackend.UNLIMITED_TIMEOUT, functionName,
                    "xiiiidxi", fpl, minLen, minToks, minFreq, maxNewVars,
                    simScore, sel.getSearchPath(),
                    GlobalParameters.getTabWidth());
        } else {
            result = backend.callWithoutParser(
                    WranglerRefactoringBackend.UNLIMITED_TIMEOUT, functionName,
                    "xiiiidxi", sel.getSearchPath(), minLen, minToks, minFreq,
                    maxNewVars, simScore, sel.getSearchPath(),
                    GlobalParameters.getTabWidth());
        }

        if (!result.isOk()) {
            throw new WranglerRpcParsingException("Rpc error");
        }
        return new DuplicateDetectionParser(result.getValue());

    }

    @Override
    protected boolean getUserInput() {
        final Shell shell = PlatformUI.getWorkbench().getDisplay()
                .getActiveShell();

        final SimilarCodeDetectionInputDialog inputd = new SimilarCodeDetectionInputDialog(
                shell, "Similar code detection...");

        inputd.open();

        simScore = inputd.getSimScore();
        minFreq = inputd.getMinFreq();
        minLen = inputd.getMinLen();
        onlyInFile = inputd.onlyinFile();
        maxNewVars = inputd.getMaxNewVars();
        minToks = inputd.getMinToks();

        return inputd.isFinished();
    }

}
