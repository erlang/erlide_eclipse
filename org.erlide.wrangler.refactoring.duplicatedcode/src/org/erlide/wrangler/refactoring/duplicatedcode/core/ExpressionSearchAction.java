package org.erlide.wrangler.refactoring.duplicatedcode.core;

import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.WranglerRefactoringBackend;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class ExpressionSearchAction extends AbstractDuplicatesSearcherAction {

	@Override
	protected IResultParser callRefactoring()
			throws WranglerRpcParsingException {
		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();
		WranglerRefactoringBackend backend = WranglerBackendManager
				.getRefactoringBackend();
		RpcResult result = backend.callWithoutParser(TIMEOUT,
				"expr_search_eclipse", "sxxi", sel.getFilePath(), sel
						.getSelectionRange().getStartPos(), sel
						.getSelectionRange().getEndPos(), GlobalParameters
						.getTabWidth());
		if (result.isOk())
			return new ExpressionSearchParser(result.getValue());
		else
			throw new WranglerRpcParsingException("RPC error");
	}

	@Override
	protected boolean getUserInput() {
		return true;
	}

}
