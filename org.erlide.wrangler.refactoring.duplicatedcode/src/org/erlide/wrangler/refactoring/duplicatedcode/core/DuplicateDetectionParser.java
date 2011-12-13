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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeInstanceElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedFileElement;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Parser class for Identical expression search refactorings
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicateDetectionParser extends AbstractDuplicatesParser {

    private final String emptyErrorMessage = "No expression found!";

    /**
     * Constructor
     * 
     * @param obj
     *            object to be parsed
     */
    public DuplicateDetectionParser(final OtpErlangObject obj) {
        super(obj);
    }

    // [{ [{ {filename(), integer(), integer()} , {filename(), integer(),
    // integer()} }], integer(), integer(), string()}]
    @Override
    public void parse(final OtpErlangObject object) {
        try {
            // TODO testing all cases
            if (object instanceof OtpErlangTuple) {
                final OtpErlangTuple objectTuple = (OtpErlangTuple) object;
                setUnSuccessful(((OtpErlangString) objectTuple.elementAt(1))
                        .stringValue());

            } else {
                final OtpErlangList resultList = (OtpErlangList) object;
                if (resultList.arity() == 0) {
                    setUnSuccessful(emptyErrorMessage);
                    return;
                }

                duplicates = new ArrayList<DuplicatedCodeElement>();
                for (int i = 0; i < resultList.arity(); ++i) {
                    duplicates.add(parseDuplicates(resultList.elementAt(i)));
                }
                isSuccessful = true;
            }
        } catch (final Exception e) {
            setUnSuccessful(e.getMessage());
        }

    }

    // { [{ {filename(), integer(), integer()} , {filename(), integer(),
    // integer()} }], integer(), integer(), string()}
    protected DuplicatedCodeElement parseDuplicates(final OtpErlangObject object)
            throws OtpErlangRangeException, WranglerException {
        final OtpErlangTuple listElementTuple = (OtpErlangTuple) object;
        final OtpErlangList duplicateCodeList = (OtpErlangList) listElementTuple
                .elementAt(0);
        final LinkedHashMap<IFile, List<DuplicatedCodeInstanceElement>> values = new LinkedHashMap<IFile, List<DuplicatedCodeInstanceElement>>();

        final OtpErlangString suggestion = (OtpErlangString) listElementTuple
                .elementAt(3);
        final String suggStr = suggestion.stringValue();

        final OtpErlangObject[] elements = duplicateCodeList.elements();

        for (int i = 0; i < elements.length; ++i) {
            OtpErlangTuple elementPair = (OtpErlangTuple) elements[i];

            String replicationFunction = "";
            final OtpErlangTuple checkable = (OtpErlangTuple) elementPair
                    .elementAt(0);
            if (checkable.elementAt(0) instanceof OtpErlangTuple) {
                final OtpErlangString repFunStr = (OtpErlangString) elementPair
                        .elementAt(1);
                replicationFunction = repFunStr.stringValue();
                elementPair = checkable;

            }
            final OtpErlangTuple firstElement = (OtpErlangTuple) elementPair
                    .elementAt(0);
            final OtpErlangTuple secondElement = (OtpErlangTuple) elementPair
                    .elementAt(1);
            final OtpErlangString fileName = (OtpErlangString) firstElement
                    .elementAt(0);
            final OtpErlangLong startLine = (OtpErlangLong) firstElement
                    .elementAt(1);
            final OtpErlangLong startCol = (OtpErlangLong) firstElement
                    .elementAt(2);
            final OtpErlangLong endLine = (OtpErlangLong) secondElement
                    .elementAt(1);
            final OtpErlangLong endCol = (OtpErlangLong) secondElement
                    .elementAt(2);

            final String fileNameStr = fileName.stringValue();
            final IFile file = WranglerUtils.getFileFromPath(fileNameStr);
            final DuplicatedCodeInstanceElement instance = new DuplicatedCodeInstanceElement(
                    file, startLine.intValue(), startCol.intValue(),
                    endLine.intValue(), endCol.intValue() + 1);
            instance.setSuggestedCode(suggStr);
            instance.setReplicationFunction(replicationFunction);
            if (values.containsKey(file)) {
                values.get(file).add(instance);
            } else {
                final ArrayList<DuplicatedCodeInstanceElement> dupList = new ArrayList<DuplicatedCodeInstanceElement>();
                dupList.add(instance);
                values.put(file, dupList);
            }
        }

        final DuplicatedCodeElement result = new DuplicatedCodeElement(values
                .entrySet().iterator().next().getValue().get(0));
        result.setSuggestedCode(suggStr);

        for (final Map.Entry<IFile, List<DuplicatedCodeInstanceElement>> entry : values
                .entrySet()) {
            final DuplicatedFileElement dupFile = new DuplicatedFileElement(
                    entry.getKey());
            dupFile.setSuggestedCode(suggStr);
            for (final DuplicatedCodeInstanceElement instance : entry
                    .getValue()) {
                dupFile.addChild(instance);
            }

            result.addChild(dupFile);
        }

        return result;
    }
}
