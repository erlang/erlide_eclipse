package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlangCode {

	private ErlangCode() {
	}

	public static boolean waitForCodeServer(final Backend backend) {
		try {
			OtpErlangObject r;
			int i = 10;
			do {
				r = backend.rpcx("erlang", "whereis", "a", "code_server");
				Thread.sleep(200);
				i--;
			} while (!(r instanceof OtpErlangPid) && i > 0);
			if (!(r instanceof OtpErlangPid)) {
				ErlLogger.error("code server did not start in time for %s",
						backend.getInfo().getName());
				return false;
			}
			ErlLogger.debug("code server started");
			return true;
		} catch (final Exception e) {
			ErlLogger.error("error starting code server for %s: %s", backend
					.getInfo().getName(), e.getMessage());
			return false;
		}
	}

	public static void addPathA(final Backend backend, final String path) {
		try {
			backend.rpc("code", "add_patha", "s", path);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void addPathZ(final Backend backend, final String path) {
		try {
			backend.rpc("code", "add_pathz", "s", path);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void removePathZ(final Backend backend, String path) {
		try {
			// workaround for bug in code:del_path
			final RpcResult rr = backend.rpc("filename", "join", "x",
					new OtpErlangList(new OtpErlangString(path)));
			if (rr.isOk()) {
				path = ((OtpErlangString) rr.getValue()).stringValue();
			}

			backend.rpc("code", "del_path", null, new OtpErlangString(path));
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void removePathA(final Backend backend, String path) {
		try {
			// workaround for bug in code:del_path
			final RpcResult rr = backend.rpc("filename", "join", "x",
					new OtpErlangList(new OtpErlangString(path)));
			if (rr.isOk()) {
				path = ((OtpErlangString) rr.getValue()).stringValue();
			}

			backend.rpc("code", "del_path", "s", path);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static RpcResult loadBinary(final Backend b, final String beamf,
			final OtpErlangBinary code) throws ErlangRpcException, RpcException {
		RpcResult result;
		try {
			result = b.rpc("code", "load_binary", "asb", beamf, beamf, code);
		} catch (final NoBackendException e) {
			return RpcResult.error("no backend");
		}
		return result;
	}

	public static void delete(final Backend fBackend, final String moduleName) {
		try {
			fBackend.rpc("code", "delete", "a", moduleName);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
