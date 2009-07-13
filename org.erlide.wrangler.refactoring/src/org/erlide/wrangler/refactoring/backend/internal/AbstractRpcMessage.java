package org.erlide.wrangler.refactoring.backend.internal;

import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.wrangler.refactoring.backend.IRpcMessage;
import org.erlide.wrangler.refactoring.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class AbstractRpcMessage implements IRpcMessage {
	protected boolean isSuccessful = false;

	protected String errorMessage = "";

	/**
	 * Parses the Erlang object and stores the result.
	 * 
	 * @param object
	 */
	public void parse(RpcResult result) {
		try {
			if (!result.isOk()) {
				org.erlide.jinterface.util.ErlLogger.error(
						"Erlide communication error: ", result);
				setUnsuccessful("Communication error occured, please try again!");
				return;
			}
			OtpErlangTuple resultTuple = (OtpErlangTuple) result.getValue();
			parseRefactoringMessage(resultTuple);

		} catch (Exception e) {
			ErlLogger.error(e);
			setUnsuccessful("Internal error occured during the refactoring.\nPlease report it!");
		}
	}

	protected abstract void parseRefactoringMessage(OtpErlangTuple resultTuple)
			throws WranglerException;

	public String getMessage() {
		return errorMessage;
	}

	public boolean isSuccessful() {
		return isSuccessful;
	}

	protected void setUnsuccessful(String errorMsg) {
		this.errorMessage = errorMsg;
		this.isSuccessful = false;
	}

	protected void setSuccessful() {
		this.errorMessage = "";
		this.isSuccessful = true;
	}
}
