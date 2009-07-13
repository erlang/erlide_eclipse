package org.erlide.wrangler.refactoring.backend.internal;



import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RefactoringRpcMessage extends AbstractRefactoringRpcMessage {

	@Override
	protected void parseRefactoringMessage(OtpErlangTuple resultTuple)
			throws WranglerRpcParsingException {
		OtpErlangObject wranglerResult = resultTuple.elementAt(1);
		if (resultTuple.elementAt(0).toString().equals("ok")) {

			if (wranglerResult instanceof OtpErlangList) {
				this.changedFiles = parseFileList((OtpErlangList) wranglerResult);
				setSuccessful();
				return;
			}
		} else {
			OtpErlangString errorMsg = (OtpErlangString) wranglerResult;
			setUnsuccessful(errorMsg.stringValue());
			return;
		}

		throw new WranglerRpcParsingException(resultTuple.toString());

	}
}
