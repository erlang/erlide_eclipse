package org.erlide.wrangler.refactoring.backend.internal;

import org.erlide.wrangler.refactoring.exception.WranglerException;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * RPC message parser class for * State data to record refactorings
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class StateDataToRecordRpcMessage extends AbstractRefactoringRpcMessage {

	protected int fieldCount;
	protected OtpErlangObject stateFuns;

	@Override
	protected void parseRefactoringMessage(OtpErlangTuple resultTuple)
			throws WranglerException {

		OtpErlangObject wranglerResult = resultTuple.elementAt(1);
		if (!resultTuple.elementAt(0).toString().equals("ok")) {
			OtpErlangString msg = (OtpErlangString) wranglerResult;
			setUnsuccessful(msg.stringValue());
		} else {
			if (wranglerResult.toString().equals("non_tuple")) {
				fieldCount = 1;
			} else {
				try {
					fieldCount = ((OtpErlangLong) ((OtpErlangTuple) wranglerResult)
							.elementAt(1)).intValue();
				} catch (OtpErlangRangeException e) {
					throw new WranglerException(e.getMessage());
				}
			}
			stateFuns = resultTuple.elementAt(2);
			setSuccessful();
		}

	}

	/**
	 * Returns the number of fields, which the refactoring will need
	 * 
	 * @return number of fields
	 */
	public int getFieldCount() {
		return fieldCount;
	}

	/**
	 * Returns StateFun Erlang object
	 * 
	 * @return statefun
	 */
	public OtpErlangObject getStateFuns() {
		return stateFuns;
	}

}
