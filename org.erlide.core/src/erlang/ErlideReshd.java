package erlang;

import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class ErlideReshd {

	public static OtpErlangPid start(Backend fBackend) {
		try {
			final OtpErlangObject r = fBackend.call("erlide_shell", "start",
					"p", fBackend.getEventPid());
			final OtpErlangPid server = (OtpErlangPid) BackendUtil.ok(r);
			return server;
		} catch (BackendException e) {
			return null;
		}
	}
}
