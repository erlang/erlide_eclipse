package erlang;

import org.erlide.core.erlang.IErlScanner;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideNoparse {

	public static OtpErlangTuple parse(final IErlScanner scanner, final IBackend b)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangTuple res = (OtpErlangTuple) b.rpcx(
				"erlide_noparse", "parse", "a", scanner
						.getScannerModuleName());
		return res;
	}

}
