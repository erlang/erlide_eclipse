package org.erlide.wrangler.refactoring.backend.internal;



import org.erlide.wrangler.refactoring.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class SideEffectRefactoringMessage extends AbstractRefactoringRpcMessage {

	boolean sideEffect = false;
	OtpErlangObject info;

	@Override
	protected void parseRefactoringMessage(OtpErlangTuple resultTuple)
			throws WranglerException {
		OtpErlangObject wranglerResult = resultTuple.elementAt(1);
		if (resultTuple.elementAt(0).toString().equals("ok")) {

			if (wranglerResult instanceof OtpErlangList) {
				this.changedFiles = parseFileList((OtpErlangList) wranglerResult);
				setSuccessful();
				return;
			}
		} else if (resultTuple.elementAt(0).toString().equals(
				"unknown_side_effect")) {
			sideEffect = true;
			setUnsuccessful("");
			info = wranglerResult;
			return;
		} else {
			OtpErlangString errorMsg = (OtpErlangString) wranglerResult;
			setUnsuccessful(errorMsg.stringValue());
			return;
		}

	}

	public boolean isUserInteractionNeeded() {
		return sideEffect;
	}

	public OtpErlangTuple getSyntaxInfo() {
		return (OtpErlangTuple) info;
	}

}
