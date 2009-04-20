package org.erlide.wrangler.refactoring.duplicatedcode.core;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.DuplicateCodeInputDialog;

import com.ericsson.otp.erlang.OtpErlangObject;

public class DuplicateDetectionAction extends AbstractDuplicatesSearcherAction {

	protected boolean onlyInfile;
	protected int minToks;
	protected int minClones;

	@SuppressWarnings("boxing")
	@Override
	protected IResultParser callRefactoring() throws BackendException,
			CoreException {
		String functionName;
		OtpErlangObject result;
		if (onlyInfile) {
			functionName = "duplicated_code_in_buffer_eclipse";
			result = backend.call("wrangler", functionName, "siii", parameter
					.getFilePath(), minToks, minClones, parameter
					.getEditorTabWidth());
		} else {
			functionName = "duplicated_code_in_dirs_eclipse";
			result = backend.call("wrangler", functionName, "xiii", parameter
					.getSearchPath(), minToks, minClones, parameter
					.getEditorTabWidth());
		}

		return new DuplicateDetectionParser(result, parameter);
	}

	@Override
	protected boolean getUserInput() {
		Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

		DuplicateCodeInputDialog inputd = new DuplicateCodeInputDialog(shell,
				"Duplicate code detection...");
		inputd.open();

		onlyInfile = inputd.onlyInFile();
		minToks = inputd.getMinToks();
		minClones = inputd.getMinClones();

		return inputd.isFinished();
	}
}
