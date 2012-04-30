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

import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.IRpcMessage;
import org.erlide.wrangler.refactoring.backend.IWranglerBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * This class handles the Erlide backends, and holds special ones for Wrangler
 * operations
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerRefactoringBackend implements IWranglerBackend {
    /**
     * Wrangler module name
     */
    final static public String MODULE = "wrangler_refacs";
    /**
     * Wrangler code inspection module name
     */
    final static public String INSPECTION_MODULE = "inspec_lib";
    final static protected String RENAME_FUNCTION = "rename_fun_eclipse";

    protected IBackend backend;
    public static final int UNLIMITED_TIMEOUT = Integer.MAX_VALUE;

    /**
     * Default constructor
     * 
     * @param backend
     *            Erlide backend
     */
    public WranglerRefactoringBackend(final IBackend backend) {
        this.backend = backend;
    }

    /**
     * Send an RPC, and allow to define a costum parser
     * 
     * @param parser
     *            parser object
     * @param functionName
     *            function name in wrangler_refacs.erl
     * @param signature
     *            parameters signature
     * @param parameters
     *            parameters array
     * @return parsed RPC message
     */
    public IRpcMessage callWithParser(final IRpcMessage parser,
            final String functionName, final String signature,
            final Object... parameters) {
        final RpcResult res = callWithoutParser(functionName, signature,
                parameters);
        parser.parse(res);
        return parser;
    }

    /**
     * Send an RPC and parses it with the default parser
     * 
     * @param functionName
     *            function name in wrangler.erl
     * @param signature
     *            parameters signature
     * @param parameters
     *            parameters in an array
     * @return parsed RPC message
     * @noreference This method is not intended to be referenced by clients.
     */
    public AbstractRefactoringRpcMessage call(final String functionName,
            final String signature, final Object... parameters) {
        final RpcResult res = callWithoutParser(functionName, signature,
                parameters);
        final AbstractRefactoringRpcMessage message = new RefactoringRpcMessage();
        message.parse(res);
        return message;
    }

    /**
     * Call an RPC without a parser
     * 
     * @param functionName
     *            function name in wrangler_refacs.erl
     * @param signature
     *            parameters signature
     * @param parameters
     *            parameters array
     * @return raw RPC result
     */
    public RpcResult callWithoutParser(final String functionName,
            final String signature, final Object... parameters) {
        /*
         * ErlLogger .info("Wrangler call: " + makeLogStr(functionName,
         * parameters)); RpcResultImpl res = backend.call_noexception(MODULE,
         * functionName, signature, parameters);
         */
        return callWithoutParser(-1, functionName, signature, parameters);
    }

    /**
     * Send an RPC without using any RpcResultImpl parser
     * 
     * @param timeout
     *            timeout for the RPC
     * @param functionName
     *            function name
     * @param signature
     *            signature for the parameters
     * @param parameters
     *            parameters
     * @return RpcResultImpl object
     */
    public RpcResult callWithoutParser(final int timeout,
            final String functionName, final String signature,
            final Object... parameters) {
        ErlLogger
                .info("Wrangler call: " + makeLogStr(functionName, parameters));
        RpcResult res;
        if (timeout < 0) {
            res = backend.call_noexception(MODULE, functionName, signature,
                    parameters);
        } else {
            res = backend.call_noexception(timeout, MODULE, functionName,
                    signature, parameters);
        }

        // ErlLogger.info("Warning: " + err);
        return res;
    }

    /**
     * Call inspection function which returns with boolean values
     * 
     * @param functionName
     *            function to call
     * @param signature
     *            signature
     * @param parameters
     *            parameters
     * @return true if the call was successful, else false
     */
    public boolean callSimpleInspection(final String functionName,
            final String signature, final Object... parameters) {
        ErlLogger.info("Wrangler inspection call: "
                + makeLogStr(functionName, parameters));
        RpcResult res;
        res = backend.call_noexception(UNLIMITED_TIMEOUT, INSPECTION_MODULE,
                functionName, signature, parameters);
        try {
            if (res.isOk()) {
                final OtpErlangAtom b = (OtpErlangAtom) res.getValue();
                return b.atomValue().equals("true")
                        || b.atomValue().equals("ok");
            }
        } catch (final Exception e) {
            e.printStackTrace();
        }
        return false;

    }

    /**
     * Call an inspection function
     * 
     * @param functionName
     *            function name
     * @param signature
     *            signature
     * @param parameters
     *            function parameters
     * @return RpcResultImpl wrapped result
     */
    public RpcResult callInspection(final String functionName,
            final String signature, final Object... parameters) {
        ErlLogger.info("Wrangler inspection call: "
                + makeLogStr(functionName, parameters));
        RpcResult res;
        res = backend.call_noexception(UNLIMITED_TIMEOUT, INSPECTION_MODULE,
                functionName, signature, parameters);
        return res;

    }

    /**
     * Gets logged info (warnings, errors) from Wrangler
     * 
     * @return log list
     */
    public RpcResult getLoggedInfo() {
        final RpcResult res = backend.call_noexception("wrangler_error_logger",
                "get_logged_info", "");
        @SuppressWarnings("unused")
        final RpcResult res2 = backend.call_noexception(
                "wrangler_error_logger", "remove_all_from_logger", "");
        return res;
    }

    protected String makeLogStr(final String function, final Object[] parameters) {
        String ret = function + "(";
        for (final Object o : parameters) {
            ret += o.toString();
            ret += ", ";
        }
        if (ret.endsWith(", ")) {
            ret = ret.substring(0, ret.length() - 2);
        }
        return ret + ")";
    }
}
