package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

	private static final String ERLIDE_NOPARSE = "erlide_noparse";

	public static OtpErlangTuple initialParse(final IdeBackend b,
			final String scannerModuleName, final String moduleFileName,
			final String initialText, final String stateDir) {
		OtpErlangTuple res = null;
		try {
			res = (OtpErlangTuple) b.rpcx(ERLIDE_NOPARSE, "initial_parse",
					"asss", scannerModuleName, moduleFileName, initialText,
					stateDir);
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return res;
	}

	public static OtpErlangTuple reparse(final IdeBackend b,
			final String scannerModuleName) {
		OtpErlangTuple res = null;
		try {
			res = (OtpErlangTuple) b.rpcx(ERLIDE_NOPARSE, "reparse", "a",
					scannerModuleName);
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return res;
	}

	public static void destroy(final IdeBackend b, final String module) {
		try {
			b.rpcx(ERLIDE_NOPARSE, "destroy", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
