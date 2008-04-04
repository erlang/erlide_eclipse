package erlang;

import org.erlide.basiccore.ErlLogger;
import org.erlide.core.ErlangPlugin;
import org.erlide.runtime.backend.BackendManager;

public class ErlideScanner {

	public static void initialScan(String module, String moduleFileName,
			String initialText) {
		final String stateDir = ErlangPlugin.getDefault().getStateLocation()
				.toString();
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"initialScan", "asss", module, moduleFileName, initialText,
					stateDir);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	@SuppressWarnings("boxing")
	public static void insertText(String module, int offset, String text) {
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"insertText", "ais", module, offset + 1, text);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	@SuppressWarnings("boxing")
	public static void removeText(String module, int offset, int length) {
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"removeText", "aii", module, offset + 1, length);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void destroy(String module) {
		try {
			BackendManager.getDefault().getIdeBackend().rpcx("erlide_scanner",
					"destroy", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
