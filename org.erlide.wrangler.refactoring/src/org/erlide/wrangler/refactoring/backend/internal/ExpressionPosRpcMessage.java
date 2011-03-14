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
import java.util.Map.Entry;

import org.eclipse.jface.text.IDocument;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.util.ErlRange;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.IRange;
import org.erlide.wrangler.refactoring.util.Range;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * RpcResultImpl parser which parses object which got from Fold expression
 * refactorings
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ExpressionPosRpcMessage extends AbstractRpcMessage {

    protected OtpErlangObject syntaxTree;
    protected HashMap<IRange, OtpErlangTuple> positionDefs;

    @Override
    protected void parseRefactoringMessage(final OtpErlangTuple resultTuple)
            throws WranglerException {
        try {
            final OtpErlangObject wranglerResult = resultTuple.elementAt(1);
            if (resultTuple.elementAt(0).toString().equals("ok")) {
                OtpErlangList posDefList;
                if (wranglerResult instanceof OtpErlangTuple) {
                    syntaxTree = ((OtpErlangTuple) wranglerResult).elementAt(0);
                    posDefList = (OtpErlangList) ((OtpErlangTuple) wranglerResult)
                            .elementAt(1);
                } else {
                    syntaxTree = null;
                    posDefList = (OtpErlangList) wranglerResult;
                }

                positionDefs = new HashMap<IRange, OtpErlangTuple>();
                final OtpErlangObject[] elements = posDefList.elements();
                for (final OtpErlangObject o : elements) {
                    final OtpErlangTuple value = (OtpErlangTuple) o;
                    final OtpErlangTuple pos = (OtpErlangTuple) value
                            .elementAt(0);
                    try {
                        positionDefs.put(new Range(pos), value);
                    } catch (final OtpErlangRangeException e) {
                        e.printStackTrace();
                        setUnsuccessful("Failed to parse the result!");
                    }
                }
                setSuccessful();

            } else {
                final OtpErlangString errorMsg = (OtpErlangString) wranglerResult;
                setUnsuccessful(errorMsg.stringValue());
                return;
            }
        } catch (final Exception e) {
            throw new WranglerRpcParsingException(resultTuple.toString());
        }

    }

    /**
     * Returns a syntax tree object which is got from Wrangler
     * 
     * @return syntax tree wrapped in an Erlang object
     */
    public OtpErlangObject getSyntaxTree() {
        return syntaxTree;
    }

    /**
     * Returns pairs which represents a selection in a module.
     * 
     * @param doc
     *            the document which containing the module
     * @return selections
     */
    public HashMap<IErlRange, OtpErlangTuple> getPositionDefinitions(
            final IDocument doc) {
        final HashMap<IErlRange, OtpErlangTuple> ret = new HashMap<IErlRange, OtpErlangTuple>();
        for (final Entry<IRange, OtpErlangTuple> r : positionDefs.entrySet()) {
            ret.put(new ErlRange(r.getKey(), doc), r.getValue());
        }

        return ret;
    }

}
