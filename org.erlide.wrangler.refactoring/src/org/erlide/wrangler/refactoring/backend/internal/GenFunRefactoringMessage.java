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

import java.util.HashMap;

import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * RpcResultImpl parser class for parsing results of a n generalise function
 * refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class GenFunRefactoringMessage extends AbstractRefactoringRpcMessage {

    /**
     * Generalise function's possible parameters names
     * 
     * 
     * @author Gyorgy Orosz
     * @version %I%, %G%
     */
    public enum GenFunReturnParameterName {
        //@formatter:off
        parName,
        funName,
        arity,
        funDefPos,
        exp,
        sideEffect,
        dupsInFun,
        logCmd,
        noOfClauses,
        dupsInClause;
        //@formatter:on
    }

    @Override
    protected void parseRefactoringMessage(final OtpErlangTuple resultTuple)
            throws WranglerException {
        final OtpErlangObject wranglerResult = resultTuple.elementAt(1);
        final String state = resultTuple.elementAt(0).toString();
        if (state.equals("ok")) {

            if (wranglerResult instanceof OtpErlangList) {
                changedFiles = parseFileList((OtpErlangList) wranglerResult);
                setSuccessful();
                return;
            }
        } else if (state.equals("error")) {
            final OtpErlangString msg = (OtpErlangString) wranglerResult;
            setUnsuccessful(msg.stringValue());
            return;

        } else if (state.equals("multiple_instances")) {
            parameters = new HashMap<GenFunReturnParameterName, OtpErlangObject>();
            final OtpErlangTuple pars = (OtpErlangTuple) wranglerResult;
            setState("", RefactoringState.MULTI_INSTANCES);
            parameters
                    .put(GenFunReturnParameterName.parName, pars.elementAt(0));
            parameters
                    .put(GenFunReturnParameterName.funName, pars.elementAt(1));
            parameters.put(GenFunReturnParameterName.arity, pars.elementAt(2));
            parameters.put(GenFunReturnParameterName.funDefPos,
                    pars.elementAt(3));
            parameters.put(GenFunReturnParameterName.exp, pars.elementAt(4));
            parameters.put(GenFunReturnParameterName.sideEffect,
                    pars.elementAt(5));
            parameters.put(GenFunReturnParameterName.dupsInFun,
                    pars.elementAt(6));
            parameters.put(GenFunReturnParameterName.logCmd, pars.elementAt(7));

        } else if (state.equals("unknown_side_effect")) {
            parameters = new HashMap<GenFunReturnParameterName, OtpErlangObject>();
            final OtpErlangTuple pars = (OtpErlangTuple) wranglerResult;
            setState("", RefactoringState.UNKNOWN_SIDE_EFFECT);

            parameters
                    .put(GenFunReturnParameterName.parName, pars.elementAt(0));
            parameters
                    .put(GenFunReturnParameterName.funName, pars.elementAt(1));
            parameters.put(GenFunReturnParameterName.arity, pars.elementAt(2));
            parameters.put(GenFunReturnParameterName.funDefPos,
                    pars.elementAt(3));
            parameters.put(GenFunReturnParameterName.exp, pars.elementAt(4));
            parameters.put(GenFunReturnParameterName.noOfClauses,
                    pars.elementAt(5));
            parameters.put(GenFunReturnParameterName.dupsInFun,
                    pars.elementAt(6));
            parameters.put(GenFunReturnParameterName.dupsInClause,
                    pars.elementAt(7));
            parameters.put(GenFunReturnParameterName.logCmd, pars.elementAt(8));

        } else if (state.equals("more_than_one_clause")) {
            parameters = new HashMap<GenFunReturnParameterName, OtpErlangObject>();
            final OtpErlangTuple pars = (OtpErlangTuple) wranglerResult;
            setState("", RefactoringState.MORE_THAN_ONE_CLAUSE);

            parameters
                    .put(GenFunReturnParameterName.parName, pars.elementAt(0));
            parameters
                    .put(GenFunReturnParameterName.funName, pars.elementAt(1));
            parameters.put(GenFunReturnParameterName.arity, pars.elementAt(2));
            parameters.put(GenFunReturnParameterName.funDefPos,
                    pars.elementAt(3));
            parameters.put(GenFunReturnParameterName.exp, pars.elementAt(4));
            parameters.put(GenFunReturnParameterName.sideEffect,
                    pars.elementAt(5));
            parameters.put(GenFunReturnParameterName.dupsInFun,
                    pars.elementAt(6));
            parameters.put(GenFunReturnParameterName.dupsInClause,
                    pars.elementAt(7));
            parameters.put(GenFunReturnParameterName.logCmd, pars.elementAt(8));
        } else {
            throw new WranglerRpcParsingException(resultTuple.toString());
        }
    }

    protected HashMap<GenFunReturnParameterName, OtpErlangObject> parameters = null;

    /**
     * Get parameters in a map
     * 
     * @return parameter name. value pairs
     */
    public HashMap<GenFunReturnParameterName, OtpErlangObject> getParameters() {
        return parameters;
    }
}
