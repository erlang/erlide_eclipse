package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public class ErlideReshd {

	@SuppressWarnings("boxing")
	public static int start(IBackend fBackend) throws ErlangRpcException,
			BackendException, RpcException, OtpErlangRangeException {
		final OtpErlangObject r = fBackend
				.rpcx("erlide_reshd", "start", "i", 0);
		final int port = ((OtpErlangLong) BackendUtil.ok(r)).intValue();
		return port;
	}

}
