package org.erlide.wrangler.refactoring.core.foldexpression;

import java.util.List;

import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.core.FileResourceChanges;
import org.erlide.wrangler.refactoring.core.RPCMessage;
import org.erlide.wrangler.refactoring.core.exception.WranglerRefactoringException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FoldExpressionRPCMessage extends RPCMessage {

	FoldExpressionRefactoring refactoring;

	public FoldExpressionRPCMessage(RpcResult result,
			FoldExpressionRefactoring foldExpressionRefactoring) {
		super(result);
		this.refactoring = foldExpressionRefactoring;
	}

	@Override
	public List<FileResourceChanges> getResult() {
		return null;
	}

	@Override
	protected void checkOkResultCases(OtpErlangTuple tuple)
			throws WranglerRefactoringException {
		/**
		 * checks if there is any error, in case of true throws an exception
		 */
		super.checkOkResultCases(tuple);
		OtpErlangTuple returnValue = (OtpErlangTuple) tuple.elementAt(1);

		OtpErlangList positions = (OtpErlangList) returnValue.elementAt(1);
		OtpErlangTuple syntaxTree = (OtpErlangTuple) returnValue.elementAt(0);
		refactoring.setFunClauseDef(syntaxTree);

		refactoring.setFoundPositions(positions);
	}
}
