package org.erlide.wrangler.refactoring.core.generalise;

import org.erlide.runtime.backend.RpcResult;
import org.erlide.wrangler.refactoring.core.RPCMessage;
import org.erlide.wrangler.refactoring.core.exception.WranglerRefactoringException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class GeneraliseRPCMessage extends RPCMessage {

	GeneraliseRefactoring refactoring;

	public final static int OK = 0;
	public final static int FREEVARS = 1;
	public final static int UNKNOWNSIDEEFFECT = 2;

	protected int generaliseStatus = OK;

	// TODO: it would be better, nicer with some thin interface
	public GeneraliseRPCMessage(RpcResult result,
			GeneraliseRefactoring refactoring) {
		super(result);
		this.refactoring = refactoring;
	}

	@Override
	protected void checkOkResultCases(OtpErlangTuple tuple)
			throws WranglerRefactoringException {
		if (tuple.elementAt(0).equals(new OtpErlangAtom("ok"))) {
			generaliseStatus = OK;
			return;
		} else if (tuple.elementAt(0).equals(new OtpErlangAtom("free_vars"))) {
			setParameters((OtpErlangList) tuple.elementAt(1));
			generaliseStatus = FREEVARS;
		} else if (tuple.elementAt(0).equals(
				new OtpErlangAtom("unknown_side_effect"))) {
			setParameters((OtpErlangList) tuple.elementAt(1));
			generaliseStatus = UNKNOWNSIDEEFFECT;
		} else {
			// OtpErlangTuple msgTuple = (OtpErlangTuple) tuple.elementAt(1);
			OtpErlangString msg = (OtpErlangString) tuple.elementAt(1);
			throw new WranglerRefactoringException(msg.stringValue());
		}
	}

	private void setParameters(OtpErlangList list) {
		refactoring.setAdditionalParameters(list.elementAt(0), list
				.elementAt(1), list.elementAt(2), list.elementAt(3), list
				.elementAt(4));
	}

	public int getGeneraliseStatus() {
		return generaliseStatus;
	}

}
