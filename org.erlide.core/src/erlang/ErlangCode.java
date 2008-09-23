package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.ExecutionBackend;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlangCode {

	public static void addPathA(final ExecutionBackend fBackend,
			final String path) {
		try {
			fBackend.rpc("code", "add_patha", "s", path);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void addPathZ(final ExecutionBackend fBackend,
			final String path) {
		try {
			fBackend.rpc("code", "add_pathz", "s", path);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void removePathZ(final ExecutionBackend fBackend, String path) {
		try {
			// workaround for bug in code:del_path
			final RpcResult rr = fBackend.rpc("filename", "join", "x",
					new OtpErlangList(new OtpErlangString(path)));
			if (rr.isOk()) {
				path = ((OtpErlangString) rr.getValue()).stringValue();
			}

			fBackend.rpc("code", "del_path", null, new OtpErlangString(path));
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void removePathA(final ExecutionBackend fBackend, String path) {
		try {
			// workaround for bug in code:del_path
			final RpcResult rr = fBackend.rpc("filename", "join", "x",
					new OtpErlangList(new OtpErlangString(path)));
			if (rr.isOk()) {
				path = ((OtpErlangString) rr.getValue()).stringValue();
			}

			fBackend.rpc("code", "del_path", "s", path);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static RpcResult loadBinary(final IBackend b, final String beamf,
			final OtpErlangBinary code) throws ErlangRpcException, RpcException {
		RpcResult result;
		try {
			result = b.rpc("code", "load_binary", "asb", beamf, beamf, code);
		} catch (final NoBackendException e) {
			return RpcResult.error("no backend");
		}
		return result;
	}

	public static void delete(final IBackend fBackend, final String moduleName) {
		try {
			fBackend.rpc("code", "delete", "a", moduleName);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

}
