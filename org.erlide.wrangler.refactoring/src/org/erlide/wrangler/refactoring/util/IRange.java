package org.erlide.wrangler.refactoring.util;

import com.ericsson.otp.erlang.OtpErlangTuple;

public interface IRange {
	public int getStartLine();

	public int getEndLine();

	public int getStartCol();

	public int getEndCol();

	public OtpErlangTuple getStartPos();

	public OtpErlangTuple getEndPos();

	public OtpErlangTuple getPos();

	public String toString();

}
