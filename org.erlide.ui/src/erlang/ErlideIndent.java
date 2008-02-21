package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideIndent {

	@SuppressWarnings("boxing")
	public static OtpErlangObject indentNextLine(final IBackend b, String txt,
			int tabw) throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangObject r1 = b.rpcx("erlide_indent", "indent_next_line",
				"si", txt, tabw);
		return r1;
	}

}
