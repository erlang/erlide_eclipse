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

import java.util.ArrayList;

import org.erlide.utils.ErlUtils;
import org.erlide.wrangler.refactoring.backend.ChangedFile;
import org.erlide.wrangler.refactoring.backend.IRefactoringRpcMessage;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Abstract class for parsing an RPC message from Wrangler.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class AbstractRefactoringRpcMessage extends AbstractRpcMessage
        implements IRefactoringRpcMessage {

    protected ArrayList<ChangedFile> changedFiles = null;

    /**
     * Returns with those files, which are changed during the refactoring.
     */
    @Override
    public ArrayList<ChangedFile> getRefactoringChangeset() {
        return changedFiles;
    }

    protected ArrayList<ChangedFile> parseFileList(final OtpErlangList fileList) {
        final ArrayList<ChangedFile> ret = new ArrayList<ChangedFile>();

        OtpErlangTuple e;
        OtpErlangString oldPath, newPath;
        for (int i = 0; i < fileList.arity(); ++i) {
            e = (OtpErlangTuple) fileList.elementAt(i);
            oldPath = (OtpErlangString) e.elementAt(0);
            newPath = (OtpErlangString) e.elementAt(1);
            final String newContent = ErlUtils.asString(e.elementAt(2));

            ret.add(new ChangedFile(oldPath.stringValue(), newPath
                    .stringValue(), newContent));
        }
        return ret;
    }

}
