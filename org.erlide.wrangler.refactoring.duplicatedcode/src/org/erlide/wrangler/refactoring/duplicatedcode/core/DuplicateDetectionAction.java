package org.erlide.wrangler.refactoring.duplicatedcode.core;

import java.io.IOException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.wrangler.refactoring.core.exception.WranglerWarningException;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.DuplicateCodeInputDialog;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangObject;

public class DuplicateDetectionAction extends AbstractDuplicatesSearcherAction {

	protected boolean onlyInfile;
	protected int minToks;
	protected int minClones;

	@SuppressWarnings("boxing")
	@Override
	protected IResultParser callRefactoring() throws BackendException,
			CoreException, IOException, WranglerWarningException {
		String functionName;
		OtpErlangObject result;

		// getting the path of the fragment

		String suffixPath = getSuffixPath();
		ErlLogger.debug("Suffix binary at: " + suffixPath);

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

	private String getSuffixPath() throws IOException, WranglerWarningException {
		Bundle[] bs = Platform
				.getFragments(Platform
						.getBundle(org.erlide.wrangler.refactoring.Activator.PLUGIN_ID));
		if (bs.length < 1) {
			ErlLogger.debug("Fragment is not loaded?! No C binary is run.");
			return "";
		}
		Bundle fragment = bs[0];
		java.net.URL url = FileLocator.find(fragment, new Path(""), null);
		url = FileLocator.resolve(url);
		IPath path = new Path(url.getPath());
		path = path.append("wrangler");
		path = path.append("bin");

		String os = Platform.getOS();
		if (os.equals(Platform.OS_LINUX)) {
			path = path.append("linux");
			path = path.append("suffixtree");
		/*} else if (os.equals(Platform.OS_WIN32)) {
			path = path.append("win32");
			path = path.append("suffixtree.exe");*/
		} else if (os.equals(Platform.OS_MACOSX)) {
			path = path.append("macosx");
			path = path.append("suffixtree");
		} else {
			ErlLogger.debug("Not supported OS found, no C binary is used.");
			return "";
		}

		return path.toOSString();
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
