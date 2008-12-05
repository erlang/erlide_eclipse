package erlang;

import java.util.List;

import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideDoc {
	public static OtpErlangObject getProposalsWithDoc(final IdeBackend b,
			final String mod, final String prefix, final String stateDir) {
		OtpErlangObject res = null;
		try {
			res = b.rpcx("erlide_otp_doc", "get_proposals", "ass", mod, prefix,
					stateDir);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	public static OtpErlangObject getModules(final IdeBackend b,
			final String prefix, final List<String> projectModules) {
		OtpErlangObject res = null;
		try {
			res = b.rpcx("erlide_otp_doc", "get_modules", "sls", prefix,
					projectModules);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangObject getDocFromScan(final IdeBackend b,
			final int offset, final String stateDir, final String module,
			final List<IErlImport> imports) {
		OtpErlangObject res = null;
		// ErlLogger.debug("getDoc:: %s %s %s", module, offset, imports);
		try {
			res = b.rpcx("erlide_otp_doc", "get_doc_from_scan_tuples", "ailos",
					module, offset, imports, stateDir);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return res;
	}

	public static String getOtpDocLoation(final IdeBackend b) {
		OtpErlangObject res = null;
		try {
			res = b.rpcx("erlide_otp_doc", "get_otp_doc_location", "");
			return Util.stringValue(res);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return "";
	}
}