package erlang;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlidePairMatch {

	@SuppressWarnings("boxing")
	public static OtpErlangObject match(final int offset, final String module)
			throws BackendException {
		OtpErlangObject r1;
		r1 = ErlangCore.getBackendManager().getIdeBackend().call(
				"erlide_pair_match", "match", "ia", offset, module);
		return r1;
	}

}
