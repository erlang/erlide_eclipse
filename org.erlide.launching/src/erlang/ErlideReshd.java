package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;

public class ErlideReshd {

	@SuppressWarnings("boxing")
	public static OtpErlangPid start(IBackend fBackend)
			throws BackendException, RpcException, OtpErlangRangeException {
		final OtpErlangObject r = fBackend.rpcx("erlide_shell", "start", "p",
				fBackend.getEventPid());
		final OtpErlangPid server = (OtpErlangPid) BackendUtil.ok(r);
		return server;
	}
}
