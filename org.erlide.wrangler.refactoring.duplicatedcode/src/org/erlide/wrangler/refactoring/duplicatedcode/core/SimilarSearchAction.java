package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.WranglerRefactoringBackend;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.SimilarSearchInputDialog;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangFloat;

/**
 * Parses the result of an rpc between Wrangler and erlide, which about finding
 * similar expressions.
 * 
 * @author György Orosz
 * 
 */
public class SimilarSearchAction extends AbstractDuplicatesSearcherAction {

	private float simScore;
	boolean onlyInFile;

	@Override
	protected IResultParser callRefactoring()
			throws WranglerRpcParsingException, CoreException, IOException,
			WranglerWarningException {

		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();
		WranglerRefactoringBackend backend = WranglerBackendManager
				.getRefactoringBackend();
		RpcResult result = null;
		String functionName;
		if (onlyInFile) {
			functionName = "simi_expr_search_in_buffer_eclipse";
		} else {
			functionName = "simi_expr_search_in_dirs_eclipse";
		}
		result = backend.callWithoutParser(TIMEOUT, functionName, "sxxxxi", sel
				.getFilePath(), sel.getSelectionRange().getStartPos(), sel
				.getSelectionRange().getEndPos(), new OtpErlangFloat(simScore),
				sel.getSearchPath(), GlobalParameters.getTabWidth());

		if (result.isOk())
			return new SimilarExpressionSearchParser(result.getValue());
		else
			throw new WranglerRpcParsingException("RPC error");
	}

	@Override
	protected boolean getUserInput() {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

		SimilarSearchInputDialog inputd = new SimilarSearchInputDialog(shell,
				"Search for similar expressions...");
		inputd.open();
		simScore = (float) inputd.getSimScore();
		onlyInFile = inputd.onlyinFile();
		return inputd.isFinished();
	}
}
