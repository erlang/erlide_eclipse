package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideOpen {

	public static String getIncludeLib(final IdeBackend b, String s)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangObject t = b.rpcx("erlide_open", "get_include_lib",
				null, s);
		if (t instanceof OtpErlangTuple) {
			final OtpErlangObject es = ((OtpErlangTuple) t).elementAt(1);
			s = ((OtpErlangString) es).stringValue();
		}
		return s;
	}

	public static OtpErlangObject getSourceFromModule(final IdeBackend b,
			final List<Tuple> pathVars, final String mod,
			final String fExternalModules) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangObject res2 = b.rpcx("erlide_open",
				"get_source_from_module", "aslx", mod, fExternalModules,
				fixPathVars(pathVars));
		return res2;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject getOpenInfo(final IdeBackend b,
			final int window, final OtpErlangList list,
			final List<OtpErlangTuple> pathVars, final String fExternalModules)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangObject res = b.rpcx("erlide_open", "open_info", "xislx",
				list, window, fExternalModules, pathVars);
		return res;
	}

	private static List<OtpErlangTuple> fixPathVars(final List<Tuple> pathVars) {
		final List<OtpErlangTuple> pathVars2 = new ArrayList<OtpErlangTuple>();
		for (final Tuple t : pathVars) {
			pathVars2.add(new OtpErlangTuple(new OtpErlangString((String) t
					.get(0)), new OtpErlangString((String) t.get(1))));
		}
		return pathVars2;
	}

	@SuppressWarnings("boxing")
	public static OpenResult open(final IdeBackend b, final String scannerName,
			final int offset, final String externalModules,
			final List<Tuple> pathVars) throws RpcException, BackendException {
		ErlLogger.debug("open offset " + offset);
		final OtpErlangObject res = b.rpcx("erlide_open", "open", "aislx",
				scannerName, offset, externalModules, fixPathVars(pathVars));
		return new OpenResult(res);
	}

	public static OtpErlangTuple findFirstVar(final IdeBackend b,
			final String name, final String source) {
		OtpErlangObject res;
		try {
			res = b.rpcx("erlide_open", "find_first_var", "ss", name, source);
			if (res instanceof OtpErlangTuple) {
				return (OtpErlangTuple) res;
			}
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return null;
	}
}
