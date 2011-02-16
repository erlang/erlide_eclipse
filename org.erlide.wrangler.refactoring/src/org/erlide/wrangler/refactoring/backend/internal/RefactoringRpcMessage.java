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

import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Simple wrapper class for Wrangler refactorings messages.
 * 
 * Suitable for those refactorings which have a simple state transition
 * containing 4 cases. First: refactoring has been called and succeeds. Second:
 * Refactoring fails after calling. Third: refactoring returns with warning
 * message, user accepts it and succeeds. Fourth: same before but after
 * accepting, refactoring fails.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class RefactoringRpcMessage extends AbstractRefactoringRpcMessage {

    OtpErlangTuple resultTuple;

    @Override
    protected void parseRefactoringMessage(final OtpErlangTuple tuple)
            throws WranglerRpcParsingException {
        resultTuple = tuple;

        final OtpErlangObject wranglerResult = tuple.elementAt(1);
        if (tuple.elementAt(0).toString().equals("ok")) {

            if (wranglerResult instanceof OtpErlangList) {
                changedFiles = parseFileList((OtpErlangList) wranglerResult);
                setSuccessful();
                return;
            }
        } else {
            final OtpErlangString msg = (OtpErlangString) wranglerResult;
            if (tuple.elementAt(0).toString().equals("warning")) {
                setWarning(msg.stringValue());
            } else if (tuple.elementAt(0).toString().equals("question")) {
                setQuestion(msg.stringValue());
            } else {
                setUnsuccessful(msg.stringValue());
            }
            return;
        }

        throw new WranglerRpcParsingException(tuple.toString());

    }

    /**
     * Returns the raw result object from Wrangler
     * 
     * @return result from Wrangler
     */
    public OtpErlangTuple getResultObject() {
        return resultTuple;
    }
}
