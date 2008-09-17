package org.erlide.wrangler.refactoring.core.foldexpression;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class ExpressionInfo {

	private OtpErlangTuple position;

	// private boolean selected;

	public ExpressionInfo(OtpErlangTuple position) {
		this.position = position;
	}

	// public String getPosExpression() {
	// return "sad";
	// }

	public OtpErlangTuple getErlangPosition() {
		return position;
	}

	public OtpErlangTuple getStartingPos() {
		OtpErlangTuple positions = (OtpErlangTuple) position.elementAt(0);
		OtpErlangTuple startPos = (OtpErlangTuple) positions.elementAt(0);
		return startPos;
	}

	public OtpErlangTuple getEndingPos() {
		OtpErlangTuple positions = (OtpErlangTuple) position.elementAt(0);
		OtpErlangTuple startPos = (OtpErlangTuple) positions.elementAt(1);
		return startPos;
	}

}
