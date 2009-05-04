package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
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
			CoreException, IOException {
		String functionName;
		OtpErlangObject result;

		// FIXME: add parameter file
		Path pluginPath = org.erlide.wrangler.refactoring.Activator
				.getPluginPath();
		String suffixPath = pluginPath.append("wrangler").append("bin")
				.toOSString();

		if (onlyInfile) {
			functionName = "duplicated_code_in_buffer_eclipse";
			result = backend.call("wrangler", functionName, "siiis", parameter
					.getFilePath(), minToks, minClones, parameter
					.getEditorTabWidth(), suffixPath);
		} else {
			functionName = "duplicated_code_in_dirs_eclipse";
			result = backend.call("wrangler", functionName, "xiiis", parameter
					.getSearchPath(), minToks, minClones, parameter
					.getEditorTabWidth(), suffixPath);
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
