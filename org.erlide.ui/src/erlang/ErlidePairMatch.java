package erlang;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlidePairMatch {

	@SuppressWarnings("boxing")
	public static OtpErlangObject match(int offset, String module)
			throws ErlangRpcException, BackendException, RpcException {
		OtpErlangObject r1;
		r1 = ErlangCore.getBackendManager().getIdeBackend().callx(
				"erlide_pair_match", "match", "ia", offset, module);
		return r1;
	}

}
