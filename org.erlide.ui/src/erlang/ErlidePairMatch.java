package erlang;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.rpc.RpcException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlidePairMatch {

	@SuppressWarnings("boxing")
	public static OtpErlangObject match(int offset, String module)
			throws RpcException {
		OtpErlangObject r1;
		r1 = ErlangCore.getBackendManager().getIdeBackend().call(
				"erlide_pair_match", "match", "ia", offset, module);
		return r1;
	}

}
