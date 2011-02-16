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
package org.erlide.wrangler.refactoring.backend.internal;

import org.erlide.wrangler.refactoring.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Rpc message parser for process related refactorings
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ProcessRpcMessage extends AbstractRefactoringRpcMessage {

    protected boolean hasUndecidables = false;

    @Override
    protected void parseRefactoringMessage(final OtpErlangTuple resultTuple)
            throws WranglerException {
        final OtpErlangObject wranglerResult = resultTuple.elementAt(1);
        if (resultTuple.elementAt(0).toString().equals("ok")) {

            if (wranglerResult instanceof OtpErlangList) {
                changedFiles = parseFileList((OtpErlangList) wranglerResult);
                setSuccessful();
                return;
            }
        } else if (resultTuple.elementAt(0).toString().equals("undecidables")) {
            hasUndecidables = true;

        }
        final OtpErlangString errorMsg = (OtpErlangString) wranglerResult;
        setUnsuccessful(errorMsg.stringValue());
        return;

    }

    /**
     * Returns true if refactoring is unsure...
     * 
     * @return true if is unsure
     */
    public boolean hasUndecidables() {
        return hasUndecidables;
    }

}
