package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

	public static OtpErlangTuple initialParse(final IBackend b,
			final String scannerModuleName, final String moduleName,
			final String moduleFileName, final String stateDir)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangTuple res = (OtpErlangTuple) b.rpcx("erlide_noparse",
				"initial_parse", "asss", scannerModuleName, moduleName,
				moduleFileName, stateDir);
		return res;
	}

	public static OtpErlangTuple reparse(final IBackend b,
			final String scannerModuleName, final String moduleName)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangTuple res = (OtpErlangTuple) b.rpcx("erlide_noparse",
				"reparse", "as", scannerModuleName, moduleName);
		return res;
	}

}
