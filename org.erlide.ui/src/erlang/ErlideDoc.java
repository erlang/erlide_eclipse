package erlang;

import java.util.Collection;
import java.util.List;

import org.erlide.core.erlang.IErlImport;
import org.erlide.jinterface.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideDoc {
	public static OtpErlangObject getProposalsWithDoc(final Backend b,
			final String mod, final String prefix, final String stateDir) {
		OtpErlangObject res = null;
		try {
			res = b.call("erlide_otp_doc", "get_proposals", "ass", mod, prefix,
					stateDir);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	public static OtpErlangObject getModules(final Backend b,
			final String prefix, final List<String> projectModules) {
		OtpErlangObject res = null;
		try {
			res = b.call("erlide_otp_doc", "get_modules", "sls", prefix,
					projectModules);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject getDocFromScan(final Backend b,
			final int offset, final String stateDir, final String module,
			final Collection<IErlImport> imports, final String externalModules,
			final OtpErlangList pathVars) {
		OtpErlangObject res = null;
		// ErlLogger.debug("getDoc:: %s %s %s", module, offset, imports);
		try {
			res = b.call("erlide_otp_doc", "get_doc_from_scan_tuples",
					"ailossx", module, offset, imports, stateDir,
					externalModules, pathVars);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	public static String getOtpDocLoation(final Backend b) {
		// OtpErlangObject res = null;
		// try {
		// // commented out since target doesn't exist
		// // res = b.rpcx("erlide_otp_doc", "get_otp_doc_location", "");
		// // return Util.stringValue(res);
		// } catch (final RpcException e) {
		// ErlLogger.warn(e);
		// } catch (final BackendException e) {
		// ErlLogger.warn(e);
		// }
		return "";
	}
}
