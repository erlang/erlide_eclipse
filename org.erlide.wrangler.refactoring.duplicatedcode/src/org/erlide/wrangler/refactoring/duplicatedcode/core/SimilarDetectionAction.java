package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.backend.WranglerRefactoringBackend;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.SimilarCodeDetectionInputDialog;
import org.erlide.wrangler.refactoring.exception.WranglerRpcParsingException;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

public class SimilarDetectionAction extends AbstractDuplicatesSearcherAction {

	int minToks;
	int minFreq;
	double simScore;
	boolean onlyInFile;

	@Override
	protected IResultParser callRefactoring()
			throws WranglerRpcParsingException, CoreException, IOException,
			WranglerWarningException {
		WranglerRefactoringBackend backend = WranglerBackendManager
				.getRefactoringBackend();
		IErlMemberSelection sel = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();

		RpcResult result;
		String functionName = "sim_code_detection_eclipse";
		if (onlyInFile) {

			OtpErlangString fp = new OtpErlangString(sel.getFilePath());
			OtpErlangString[] fpa = new OtpErlangString[1];
			fpa[0] = fp;
			OtpErlangList fpl = new OtpErlangList(fpa);

			result = backend.callWithoutParser(TIMEOUT, functionName, "xiidxi",
					fpl, minToks, minFreq, simScore, sel.getSearchPath(),
					GlobalParameters.getTabWidth());
		} else {
			result = backend.callWithoutParser(TIMEOUT, functionName, "xiidxi",
					sel.getSearchPath(), minToks, minFreq, simScore, sel
							.getSearchPath(), GlobalParameters.getTabWidth());
		}

		if (!result.isOk())
			throw new WranglerRpcParsingException("Rpc error");
		return new DuplicateDetectionParser(result.getValue());

	}

	@Override
	protected boolean getUserInput() {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

		SimilarCodeDetectionInputDialog inputd = new SimilarCodeDetectionInputDialog(
				shell, "Similar code detection...");

		inputd.open();

		simScore = inputd.getSimScore();
		minFreq = inputd.getMinFreq();
		minToks = inputd.getMinToks();
		onlyInFile = inputd.onlyinFile();

		return inputd.isFinished();
	}

}
