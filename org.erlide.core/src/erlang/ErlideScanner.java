package erlang;

import org.erlide.basiccore.ErlLogger;
import org.erlide.core.ErlangPlugin;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

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
			OtpErlangObject r = BackendManager.getDefault().getIdeBackend()
					.rpcx("erlide_scanner", "insertText", "ais", module,
							offset + 1, text);
			if (r instanceof OtpErlangTuple) {
				ErlLogger.error("GOT::" + r.toString());
			}
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (RpcException e) {
		} catch (BackendException e) {
		}
	}

	@SuppressWarnings("boxing")
	public static void removeText(String module, int offset, int length) {
		try {
			OtpErlangObject r = BackendManager.getDefault().getIdeBackend()
					.rpcx("erlide_scanner", "removeText", "aii", module,
							offset + 1, length);
			if (r instanceof OtpErlangTuple) {
				ErlLogger.error("GOT::" + r.toString());
			}
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (RpcException e) {
		} catch (BackendException e) {
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
