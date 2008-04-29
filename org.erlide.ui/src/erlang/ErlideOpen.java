package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideOpen {

	public static String getIncludeLib(String s) throws ErlangRpcException,
			BackendException, RpcException {
		final IBackend b = BackendManager.getDefault().getIdeBackend();
		final OtpErlangObject t = b.rpcx("erlide_open", "get_include_lib",
				null, s);
		if (t instanceof OtpErlangTuple) {
			final OtpErlangObject es = ((OtpErlangTuple) t).elementAt(1);
			s = ((OtpErlangString) es).stringValue();
		}
		return s;
	}

	public static OtpErlangObject getSourceFromModule(final IBackend b,
			final List<Tuple> pathVars, final String mod,
			String fExternalModules) throws ErlangRpcException,
			BackendException, RpcException {
		List<OtpErlangTuple> pathVars2 = new ArrayList<OtpErlangTuple>();
		for (Tuple t : pathVars) {
			pathVars2.add(new OtpErlangTuple(new OtpErlangObject[] {
					(OtpErlangObject) t.get(0), (OtpErlangObject) t.get(1) }));
		}
		final OtpErlangObject res2 = b.rpcx("erlide_open",
				"get_source_from_module", "aslx", mod, fExternalModules,
				pathVars2);
		return res2;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject getOpenInfo(final IBackend b, int window,
			final OtpErlangList list, final List<OtpErlangTuple> pathVars,
			String fExternalModules) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangObject res = b.rpcx("erlide_open", "open_info", "xislx",
				list, window, fExternalModules, pathVars);
		return res;
	}

	@SuppressWarnings("boxing")
	public static OpenResult open(final IBackend b, String scannerName,
			int offset, String externalModules, List<Tuple> pathVars)
			throws RpcException, BackendException {
		List<OtpErlangTuple> pathVars2 = new ArrayList<OtpErlangTuple>();
		for (Tuple t : pathVars) {
			pathVars2.add(new OtpErlangTuple(new OtpErlangObject[] {
					(OtpErlangObject) t.get(0), (OtpErlangObject) t.get(1) }));
		}
		final OtpErlangObject res = b.rpcx("erlide_open", "open", "aislx",
				scannerName, offset, externalModules, pathVars2);
		return new OpenResult(res);
	}

}
