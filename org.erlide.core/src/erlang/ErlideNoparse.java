package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

	public static OtpErlangTuple initialParse(final IdeBackend b,
			final String scannerModuleName, final String moduleFileName,
			final String initialText, final String stateDir) {
		OtpErlangTuple res = null;
		try {
			res = (OtpErlangTuple) b.rpcx("erlide_noparse", "initial_parse",
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
			res = (OtpErlangTuple) b.rpcx("erlide_noparse", "reparse", "a",
					scannerModuleName);
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return res;
	}

}
