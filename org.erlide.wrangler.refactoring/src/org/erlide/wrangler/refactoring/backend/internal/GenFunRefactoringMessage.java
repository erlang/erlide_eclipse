package org.erlide.wrangler.refactoring.backend.internal;

import java.util.HashMap;
import java.util.Map;

import org.erlide.wrangler.refactoring.backend.RefactoringState;
import org.erlide.wrangler.refactoring.exception.WranglerException;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class GenFunRefactoringMessage extends AbstractRefactoringRpcMessage {

	@Override
	protected void parseRefactoringMessage(OtpErlangTuple resultTuple)
			throws WranglerException {
		OtpErlangObject wranglerResult = resultTuple.elementAt(1);
		String state = resultTuple.elementAt(0).toString();
		if (state.equals("ok")) {

			if (wranglerResult instanceof OtpErlangList) {
				this.changedFiles = parseFileList((OtpErlangList) wranglerResult);
				setSuccessful();
				return;
			}
		} else if (state.equals("error")) {
			OtpErlangString msg = (OtpErlangString) wranglerResult;
			setUnsuccessful(msg.stringValue());
			return;

		} else if (state.equals("multiple_instances")) {
			parameters = new HashMap<String, OtpErlangObject>();
			OtpErlangTuple pars = (OtpErlangTuple) (((OtpErlangTuple) wranglerResult)
					.elementAt(1));
			setState("", RefactoringState.MULTI_INSTANCES);

			parameters.put("parName", pars.elementAt(0));
			parameters.put("funName", pars.elementAt(1));
			parameters.put("arity", pars.elementAt(2));
			parameters.put("funDefPos", pars.elementAt(3));
			parameters.put("exp", pars.elementAt(4));
			parameters.put("sideEffect", pars.elementAt(5));
			parameters.put("dupsInFun", pars.elementAt(6));
			parameters.put("logCmd", pars.elementAt(7));

		} else if (state.equals("unknown_side_effect")) {
			parameters = new HashMap<String, OtpErlangObject>();
			OtpErlangTuple pars = (OtpErlangTuple) (((OtpErlangTuple) wranglerResult)
					.elementAt(1));
			setState("", RefactoringState.UNKNOWN_SIDE_EFFECT);

			parameters.put("parName", pars.elementAt(0));
			parameters.put("funName", pars.elementAt(1));
			parameters.put("arity", pars.elementAt(2));
			parameters.put("funDefPos", pars.elementAt(3));
			parameters.put("exp", pars.elementAt(4));
			parameters.put("noOfClauses", pars.elementAt(5));
			parameters.put("dupsInFun", pars.elementAt(6));
			parameters.put("dupsInClause", pars.elementAt(7));
			parameters.put("logCmd", pars.elementAt(8));

		} else if (state.equals("more_than_one_clause")) {
			parameters = new HashMap<String, OtpErlangObject>();
			OtpErlangTuple pars = (OtpErlangTuple) (((OtpErlangTuple) wranglerResult)
					.elementAt(1));
			setState("", RefactoringState.MORE_THAN_ONE_CLAUSE);

			parameters.put("parName", pars.elementAt(0));
			parameters.put("funName", pars.elementAt(1));
			parameters.put("arity", pars.elementAt(2));
			parameters.put("funDefPos", pars.elementAt(3));
			parameters.put("exp", pars.elementAt(4));
			parameters.put("sideEffect", pars.elementAt(5));
			parameters.put("dupsInFun", pars.elementAt(6));
			parameters.put("dupsInClause", pars.elementAt(7));
			parameters.put("logCmd", pars.elementAt(8));

		}

		throw new WranglerRpcParsingException(resultTuple.toString());
	}

	protected Map<String, OtpErlangObject> parameters = null;

	public Map<String, OtpErlangObject> getParameters() {
		return parameters;
	}
}
