package org.erlide.wrangler.refactoring.core.functiontoprocess;

import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.core.RPCMessage;
import org.erlide.wrangler.refactoring.core.exception.WranglerException;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;

import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ProcessRPCMessage extends RPCMessage {

	public ProcessRPCMessage(RpcResult result) {
		super(result);
	}

	String undecidables;

	public String getUndecidabels() {
		return undecidables;
	}

	@Override
	protected void checkOkResultCases(OtpErlangTuple tuple)
			throws WranglerException {
		if (tuple.elementAt(0).toString().equals("undecidables")) {
			OtpErlangString msg = (OtpErlangString) tuple.elementAt(1);
			undecidables = msg.stringValue();
			throw new WranglerWarningException(msg.stringValue());
		}
		// continue as usual
		super.checkOkResultCases(tuple);
	}
}
