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

public class Range implements IRange {
	protected int startLine, startCol, endLine, endCol;

	public Range(int startLine, int startCol, int endLine, int endCol) {
		this.startLine = startLine;
		this.startCol = startCol;
		this.endLine = endLine;
		this.endCol = endCol;
	}

	public Range(OtpErlangTuple position) throws OtpErlangRangeException {
		this((OtpErlangTuple) position.elementAt(0), (OtpErlangTuple) position
				.elementAt(1));
	}

	public Range(OtpErlangTuple startPos, OtpErlangTuple endPos)
			throws OtpErlangRangeException {
		this(((OtpErlangLong) startPos.elementAt(0)).intValue(),
				((OtpErlangLong) startPos.elementAt(1)).intValue(),
				((OtpErlangLong) endPos.elementAt(0)).intValue(),
				((OtpErlangLong) endPos.elementAt(1)).intValue());
	}

	public int getEndCol() {
		return endCol;
	}

	public int getEndLine() {
		return endLine;
	}

	public int getStartCol() {
		return startCol;
	}

	public int getStartLine() {
		return startLine;
	}

	public OtpErlangTuple getStartPos() {
		return OtpErlang.mkTuple(new OtpErlangInt(startLine),
				new OtpErlangInt(startCol));
	}

	public OtpErlangTuple getEndPos() {
		return OtpErlang.mkTuple(new OtpErlangInt(endLine),
				new OtpErlangInt(endCol));
	}

	@Override
	public String toString() {
		return "{" + getStartLine() + "," + getStartCol() + "}" + "-" + "{"
				+ getEndLine() + "," + getEndCol() + "}";
	}

	public OtpErlangTuple getPos() {
		return OtpErlang.mkTuple(getStartPos(), getEndPos());
	}
}
