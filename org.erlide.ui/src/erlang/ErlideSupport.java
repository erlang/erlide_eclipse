package erlang;

import org.eclipse.jface.text.ITextSelection;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideSupport {

	@SuppressWarnings("boxing")
	public
	static OtpErlangObject call(String module, String fun,
			ITextSelection selection, String text) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangObject r1 = BackendManager.getDefault().getIdeBackend()
				.rpcx(module, fun, "si", text, selection.getOffset());
		return r1;
	}

}
