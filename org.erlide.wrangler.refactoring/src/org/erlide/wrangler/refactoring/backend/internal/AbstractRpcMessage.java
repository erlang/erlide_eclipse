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

import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.IRpcMessage;
import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Abstract class for parsing RpcResultImpl objects
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class AbstractRpcMessage implements IRpcMessage {
    protected RefactoringState refactoringState = RefactoringState.ERROR;

    protected String messageString = "";

    /**
     * Parses the Erlang object and stores the result.
     * 
     * @param result
     *            input object to be parsed
     */
    @Override
    public void parse(final RpcResult result) {
        try {
            if (!result.isOk()) {
                org.erlide.jinterface.ErlLogger.error(
                        "Erlide communication error: ", result);
                setUnsuccessful("Communication error occured, please try again!");
                ErlLogger.error(result.toString());
                return;
            }
            final OtpErlangTuple resultTuple = (OtpErlangTuple) result
                    .getValue();
            parseRefactoringMessage(resultTuple);

        } catch (final Exception e) {
            ErlLogger.error(e);
            setUnsuccessful("Internal error occured during the refactoring.\nPlease report it!");
        }
    }

    protected abstract void parseRefactoringMessage(OtpErlangTuple resultTuple)
            throws WranglerException;

    @Override
    public String getMessageString() {
        return messageString;
    }

    @Override
    public boolean isSuccessful() {
        return refactoringState == RefactoringState.OK;
    }

    @Override
    public RefactoringState getRefactoringState() {
        return refactoringState;
    }

    protected void setUnsuccessful(final String errorMsg) {
        messageString = errorMsg;
        refactoringState = RefactoringState.ERROR;
    }

    protected void setWarning(final String message) {
        messageString = message;
        refactoringState = RefactoringState.WARNING;
    }

    protected void setQuestion(final String message) {
        messageString = message;
        refactoringState = RefactoringState.QUESTION;
    }

    protected void setSuccessful() {
        messageString = "";
        refactoringState = RefactoringState.OK;
    }

    protected void setState(final String message, final RefactoringState state) {
        messageString = message;
        refactoringState = state;
    }
}
