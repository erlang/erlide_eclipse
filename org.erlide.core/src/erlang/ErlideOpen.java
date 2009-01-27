package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.JInterfaceFactory;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.Tuple;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideOpen {

	public static String getIncludeLib(final Backend b, String s)
			throws ErlangRpcException, BackendException, RpcException {
		final OtpErlangObject t = b.rpcx("erlide_open", "get_include_lib", "s",
				s);
		if (t instanceof OtpErlangTuple) {
			final OtpErlangObject es = ((OtpErlangTuple) t).elementAt(1);
			s = ((OtpErlangString) es).stringValue();
		}
		return s;
	}

	public static OtpErlangObject getSourceFromModule(final Backend b,
			final List<Tuple> pathVars, final String mod,
			final String externalModules) throws ErlangRpcException,
			BackendException, RpcException {
		final OtpErlangObject res2 = b.rpcx("erlide_open",
				"get_source_from_module", "aslx", mod, externalModules,
				fixPathVars(pathVars));
		return res2;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject getOpenInfo(final Backend b,
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
			pathVars2
					.add(JInterfaceFactory.mkTuple(new OtpErlangString(
							(String) t.get(0)), new OtpErlangString((String) t
							.get(1))));
		}
		return pathVars2;
	}

	@SuppressWarnings("boxing")
	public static OpenResult open(final Backend b, final String scannerName,
			final int offset, final String externalModules,
			final List<Tuple> pathVars) throws RpcException, BackendException {
		ErlLogger.debug("open offset " + offset);
		final OtpErlangObject res = b.rpcx("erlide_open", "open", "aislx",
				scannerName, offset, externalModules, fixPathVars(pathVars));
		return new OpenResult(res);
	}

	public static OtpErlangTuple findFirstVar(final Backend b,
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

	public static List<String> getExternalModules(final Backend b,
			final String prefix, final String externalModules,
			final List<Tuple> pathVars) {
		try {
			final OtpErlangObject res = b.rpcx("erlide_open",
					"get_external_modules", "sslx", prefix, externalModules,
					fixPathVars(pathVars));
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				final List<String> result = new ArrayList<String>(l.arity());
				for (final OtpErlangObject i : l.elements()) {
					result.add(Util.stringValue(i));
				}
				return result;
			}
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return new ArrayList<String>();
	}

	public static String getExternalInclude(final Backend b,
			final String filePath, final String externalIncludes,
			final List<Tuple> pathVars) {
		try {
			final OtpErlangObject res = b.rpcx("erlide_open",
					"get_external_include", "sslx", filePath, externalIncludes,
					fixPathVars(pathVars));
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				return Util.stringValue(t.elementAt(1));
			}
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return null;
	}

	public static String getExternalModule(final Backend b, final String mod,
			final String externalModules, final List<Tuple> pathVars) {
		try {
			final OtpErlangObject res = b.rpcx("erlide_open",
					"get_external_module", "sslx", mod, externalModules,
					fixPathVars(pathVars));
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				return Util.stringValue(t.elementAt(1));
			}
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return null;
	}
}
