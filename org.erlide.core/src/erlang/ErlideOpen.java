package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideOpen {

	public static String getIncludeLib(final Backend b, String s)
			throws BackendException {
		final OtpErlangObject t = b.call("erlide_open", "get_include_lib", "s",
				s);
		if (t instanceof OtpErlangTuple) {
			final OtpErlangObject es = ((OtpErlangTuple) t).elementAt(1);
			s = ((OtpErlangString) es).stringValue();
		}
		return s;
	}

	public static OtpErlangObject getSourceFromModule(final Backend b,
			final OtpErlangList pathVars, final String mod,
			final String externalModules) throws BackendException {
		final OtpErlangObject res2 = b
				.call("erlide_open", "get_source_from_module", "asx", mod,
						externalModules, pathVars);
		return res2;
	}

	@SuppressWarnings("boxing")
	public static OpenResult getOpenInfo(final Backend b, final String s,
			final List<OtpErlangObject> imports, final String externalModules,
			final OtpErlangList pathVars) throws BackendException {
		final OtpErlangObject res = b.call("erlide_open", "open_info", "slxsx",
				s, imports, externalModules, pathVars);
		return new OpenResult(res);
	}

	@SuppressWarnings("boxing")
	public static OpenResult open(final Backend b, final String scannerName,
			final int offset, final List<OtpErlangObject> imports,
			final String externalModules, final OtpErlangList pathVars)
			throws BackendException {
		ErlLogger.debug("open offset " + offset);
		final OtpErlangObject res = b.call("erlide_open", "open", "ailxsx",
				scannerName, offset, imports, externalModules, pathVars);
		return new OpenResult(res);
	}

	public static OtpErlangTuple findFirstVar(final Backend b,
			final String name, final String source) {
		OtpErlangObject res;
		try {
			res = b.call("erlide_open", "find_first_var", "as", name, source);
			if (res instanceof OtpErlangTuple) {
				return (OtpErlangTuple) res;
			}
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return null;
	}

	public static List<String> getExternalModules(final Backend b,
			final String prefix, final String externalModules,
			final OtpErlangList pathVars) {
		try {
			final OtpErlangObject res = b.call("erlide_open",
					"get_external_modules", "ssx", prefix, externalModules,
					pathVars);
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				final List<String> result = new ArrayList<String>(l.arity());
				for (final OtpErlangObject i : l.elements()) {
					result.add(Util.stringValue(i));
				}
				return result;
			}
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return new ArrayList<String>();
	}

	public static String getExternalInclude(final Backend b,
			final String filePath, final String externalIncludes,
			final OtpErlangList pathVars) {
		try {
			final OtpErlangObject res = b.call("erlide_open",
					"get_external_include", "ssx", filePath, externalIncludes,
					pathVars);
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				return Util.stringValue(t.elementAt(1));
			}
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return null;
	}

	public static String getExternalModule(final Backend b, final String mod,
			final String externalModules, final OtpErlangList pathVars) {
		try {
			final OtpErlangObject res = b.call("erlide_open",
					"get_external_module", "ssx", mod, externalModules,
					pathVars);
			if (Util.isOk(res)) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				return Util.stringValue(t.elementAt(1));
			}
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return null;
	}
}
