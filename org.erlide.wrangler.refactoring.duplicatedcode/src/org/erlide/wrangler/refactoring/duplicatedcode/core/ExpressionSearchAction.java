package org.erlide.wrangler.refactoring.duplicatedcode.core;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.runtime.backend.BackendManager;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ExpressionSearchAction extends AbstractDuplicatesSearcherAction {

	@SuppressWarnings("boxing")
	@Override
	protected IResultParser callRefactoring() throws BackendException {
		Backend backend = BackendManager.getDefault().getIdeBackend();
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
