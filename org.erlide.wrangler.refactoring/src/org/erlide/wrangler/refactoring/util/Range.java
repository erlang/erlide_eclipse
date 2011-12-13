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
package org.erlide.wrangler.refactoring.util;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Selection range in text documents.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class Range implements IRange {
    protected int startLine, startCol, endLine, endCol;

    /**
     * Constructor with range starting and ending positions
     * 
     * @param startLine
     *            start line
     * @param startCol
     *            start column
     * @param endLine
     *            end line
     * @param endCol
     *            end column
     */
    public Range(final int startLine, final int startCol, final int endLine,
            final int endCol) {
        this.startLine = startLine;
        this.startCol = startCol;
        this.endLine = endLine;
        this.endCol = endCol;
    }

    /**
     * Constructor with position in an Erlang tuple
     * 
     * @param position
     *            {{StartingLine, StartingColumn},{EndingLine, EndingColumn}}
     * @throws OtpErlangRangeException
     *             if the given tuple is not well formed
     */
    public Range(final OtpErlangTuple position) throws OtpErlangRangeException {
        this((OtpErlangTuple) position.elementAt(0), (OtpErlangTuple) position
                .elementAt(1));
    }

    /**
     * Constructor with positions in Erlang tuples
     * 
     * @param startPos
     *            {StartingLine, StartingColumn}
     * @param endPos
     *            {EndingLine, EndingColumn}
     * @throws OtpErlangRangeException
     *             if the given tuples are not well-formed
     */
    public Range(final OtpErlangTuple startPos, final OtpErlangTuple endPos)
            throws OtpErlangRangeException {
        this(((OtpErlangLong) startPos.elementAt(0)).intValue(),
                ((OtpErlangLong) startPos.elementAt(1)).intValue(),
                ((OtpErlangLong) endPos.elementAt(0)).intValue(),
                ((OtpErlangLong) endPos.elementAt(1)).intValue());
    }

    @Override
    public int getEndCol() {
        return endCol;
    }

    @Override
    public int getEndLine() {
        return endLine;
    }

    @Override
    public int getStartCol() {
        return startCol;
    }

    @Override
    public int getStartLine() {
        return startLine;
    }

    @Override
    public OtpErlangTuple getStartPos() {
        return OtpErlang.mkTuple(new OtpErlangInt(startLine), new OtpErlangInt(
                startCol));
    }

    @Override
    public OtpErlangTuple getEndPos() {
        return OtpErlang.mkTuple(new OtpErlangInt(endLine), new OtpErlangInt(
                endCol));
    }

    @Override
    public String toString() {
        return "{" + getStartLine() + "," + getStartCol() + "}" + "-" + "{"
                + getEndLine() + "," + getEndCol() + "}";
    }

    @Override
    public OtpErlangTuple getPos() {
        return OtpErlang.mkTuple(getStartPos(), getEndPos());
    }
}
