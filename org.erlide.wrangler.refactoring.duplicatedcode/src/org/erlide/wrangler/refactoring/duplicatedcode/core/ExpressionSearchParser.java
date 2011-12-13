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

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;
import org.erlide.wrangler.refactoring.selection.IErlSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Parser class for expression search refactoring
 * 
 * @author Gyorgy Orosz
 * 
 */
public class ExpressionSearchParser extends AbstractDuplicatesParser {

    /**
     * Constructor
     * 
     * @param obj
     *            object to be parsed
     */
    public ExpressionSearchParser(final OtpErlangObject obj) {
        super(obj);
    }

    @Override
    public void parse(final OtpErlangObject object) {
        try {
            final OtpErlangTuple res = (OtpErlangTuple) object;

            if (!res.elementAt(0).toString().equals("ok")) {
                setUnSuccessful(((OtpErlangString) res.elementAt(1))
                        .stringValue());
                return;
            }
            if (res.elementAt(1).equals(new OtpErlangList())) {
                setUnSuccessful("No more instances found!");
                return;
            }

            final OtpErlangList posList = (OtpErlangList) res.elementAt(1);
            OtpErlangTuple actPos;
            OtpErlangLong startLine, startColumn, endLine, endColumn;

            final ArrayList<DuplicatedCodeInstanceElement> instances = new ArrayList<DuplicatedCodeInstanceElement>();

            final Iterator<OtpErlangObject> it = posList.iterator();
            while (it.hasNext()) {
                actPos = (OtpErlangTuple) it.next();
                startLine = (OtpErlangLong) ((OtpErlangTuple) actPos
                        .elementAt(0)).elementAt(0);
                startColumn = (OtpErlangLong) ((OtpErlangTuple) actPos
                        .elementAt(0)).elementAt(1);
                endLine = (OtpErlangLong) ((OtpErlangTuple) actPos.elementAt(1))
                        .elementAt(0);
                endColumn = (OtpErlangLong) ((OtpErlangTuple) actPos
                        .elementAt(1)).elementAt(1);

                final IErlSelection sel = GlobalParameters
                        .getWranglerSelection();
                instances.add(new DuplicatedCodeInstanceElement((IFile) sel
                        .getErlElement().getResource(), startLine.intValue(),
                        startColumn.intValue(), endLine.intValue(), endColumn
                                .intValue() + 1));
            }

            final DuplicatedCodeInstanceElement defaultInstance = instances
                    .get(0);

            final DuplicatedCodeElement result = new DuplicatedCodeElement(
                    defaultInstance);

            for (final DuplicatedCodeInstanceElement instance : instances) {
                result.addChild(instance);
            }

            isSuccessful = true;
            errorMessage = null;
            duplicates = new ArrayList<DuplicatedCodeElement>();
            duplicates.add(result);

        } catch (final Exception e) {
            setUnSuccessful(e.getMessage());
        }

    }
}
