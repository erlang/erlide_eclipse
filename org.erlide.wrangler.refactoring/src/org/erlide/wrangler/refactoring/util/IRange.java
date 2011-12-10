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

import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * Interface for defining range in text documents.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IRange {
    /**
     * Get the starting line of the range.
     * 
     * @return starting line
     */
    public int getStartLine();

    /**
     * Get the ending line of the range.
     * 
     * @return ending line
     */
    public int getEndLine();

    /**
     * Get the starting column of the range.
     * 
     * @return starting column
     */
    public int getStartCol();

    /**
     * Get the ending column of the range.
     * 
     * @return ending column
     */
    public int getEndCol();

    /**
     * Get an Erlang tuple containing the starting position. {Line, Column}
     * 
     * @return starting position
     */
    public OtpErlangTuple getStartPos();

    /**
     * Get an Erlang tuple containing the ending position. {Line, Column}
     * 
     * @return ending position
     */
    public OtpErlangTuple getEndPos();

    /**
     * Get an Erlang tuple containing the range start and end position.
     * {{StartingLine, StartingColumn},{EndingLine, EndingColumn}}
     * 
     * @return range tuple
     */
    public OtpErlangTuple getPos();

    @Override
    public String toString();

}
