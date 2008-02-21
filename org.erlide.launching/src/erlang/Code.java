package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

public class Code {

	public static void addPathA(IBackend fBackend, String path) {
		try {
			fBackend.rpc("code", "add_patha", "s", path);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public static void addPathZ(IBackend fBackend, String path) {
		try {
			fBackend.rpc("code", "add_pathz", "s", path);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public static void removePathZ_(IBackend fBackend, String path) {
		try {
			// workaround for bug in code:del_path
			RpcResult rr = fBackend.rpc("filename", "join", "x",
					new OtpErlangList(new OtpErlangString(path)));
			if (rr.isOk()) {
				path = ((OtpErlangString) rr.getValue()).stringValue();
			}
	
			fBackend.rpc("code", "del_path", null, new OtpErlangString(path));
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public static void removePathA_(IBackend fBackend, String path) {
		try {
			// workaround for bug in code:del_path
			RpcResult rr = fBackend.rpc("filename", "join", "ls",
					new OtpErlangList(new OtpErlangString(path)));
			if (rr.isOk()) {
				path = ((OtpErlangString) rr.getValue()).stringValue();
			}
	
			fBackend.rpc("code", "del_path", "s", path);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public static RpcResult loadBinary(final String beamf,
			final OtpErlangBinary code, final IBackend b)
			throws ErlangRpcException, RpcException {
		final RpcResult result = b.rpc("code", "load_binary", "asb", beamf,
				beamf, code);
		return result;
	}

	public static void delete(IBackend fBackend, String moduleName) {
		try {
			fBackend.rpc("code", "delete", "a", moduleName);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

}
