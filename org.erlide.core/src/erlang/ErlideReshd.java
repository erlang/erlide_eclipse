package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd {

	public static OtpErlangPid start(Backend fBackend) throws BackendException,
			RpcException {
		try {
			final OtpErlangObject r = fBackend.call("erlide_shell", "start",
					"p", fBackend.getEventPid());
			final OtpErlangPid server = (OtpErlangPid) BackendUtil.ok(r);
			return server;
		} catch (NoBackendException e) {
			return null;
		}
	}
}
