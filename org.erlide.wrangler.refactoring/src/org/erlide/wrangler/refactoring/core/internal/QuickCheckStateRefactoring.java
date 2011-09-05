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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.erlide.wrangler.refactoring.backend.internal.StateDataToRecordRpcMessage;
import org.erlide.wrangler.refactoring.core.SimpleOneStepWranglerRefactoring;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * Abstract refactoring for QuickCheck state data to record refactorings
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class QuickCheckStateRefactoring extends
        SimpleOneStepWranglerRefactoring {

    protected OtpErlangObject stateFuns;
    protected int fieldCount = 0;
    private String recordName;
    private List<String> fieldsNames;

    @Override
    public RefactoringStatus checkInitialConditions(final IProgressMonitor pm)
            throws CoreException, OperationCanceledException {
        // FIXME: what kind of preconditions do I need?
        final IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
                .getWranglerSelection();
        final StateDataToRecordRpcMessage message = runFirst(sel);
        if (!message.isSuccessful()) {
            return RefactoringStatus.createFatalErrorStatus(message
                    .getMessageString());
        } else {
            fieldCount = message.getFieldCount();
            stateFuns = message.getStateFuns();
            return new RefactoringStatus();
        }
    }

    /**
     * @noreference This method is not intended to be referenced by clients.
     */
    protected abstract StateDataToRecordRpcMessage runFirst(
            IErlMemberSelection sel);

    /**
     * Returns the count of fields which a new record may need
     * 
     * @return coutn of fields
     */
    public int getRecordFieldCount() {
        return fieldCount;
    }

    /**
     * Record data setter
     * 
     * @param name
     *            record name
     * @param fieldNames
     *            field names
     */
    public void setRecordData(final String name, final List<String> fieldNames) {
        recordName = name;
        fieldsNames = fieldNames;
    }

    protected String getRecordName() {
        return recordName;
    }

    protected OtpErlangList getFieldsName() {
        final ArrayList<OtpErlangString> f = new ArrayList<OtpErlangString>();
        for (final String s : fieldsNames) {
            f.add(new OtpErlangString(s));
        }
        return new OtpErlangList(f.toArray(new OtpErlangString[0]));

    }
}
