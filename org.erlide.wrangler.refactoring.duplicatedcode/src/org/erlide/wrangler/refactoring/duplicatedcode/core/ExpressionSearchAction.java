package org.erlide.wrangler.refactoring.duplicatedcode.core;

import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ExpressionSearchAction extends AbstractDuplicatesSearcherAction {

	@Override
	protected IResultParser callRefactoring() throws BackendException {

		OtpErlangObject result = backend.call("wrangler", "expression_search",
				"sxxi", parameter.getFilePath(), parameter.getStartPos(),
				parameter.getEndPos(), parameter.getEditorTabWidth());

		return new ExpressionSearchParser(result, parameter);
	}

	@Override
	protected boolean getUserInput() {
		return true;
	}

}
