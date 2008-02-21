package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideDebug {

	@SuppressWarnings("boxing")
	public
	static OtpErlangList getProcesses(IBackend fBackend,
			boolean fShowSystemProcesses, boolean fShowErlideProcesses)
			throws ErlangRpcException, BackendException, RpcException {
		OtpErlangList procs;
		procs = (OtpErlangList) BackendUtil.ok(fBackend.rpcx("erlide_debug",
				"processes", "bb", fShowSystemProcesses, fShowErlideProcesses));
		return procs;
	}

	public static OtpErlangPid startDebug(IBackend b, String mod, String func)
			throws ErlangRpcException, BackendException, RpcException {
		OtpErlangObject res;
		res = b.rpcx("erlide_debug", "start_debug", "aa", mod, func);
		final OtpErlangPid pid = (OtpErlangPid) BackendUtil.ok(res);
		return pid;
	}

	public static OtpErlangObject getProcessInfo(IBackend b, OtpErlangPid pid,
			String item) {
		OtpErlangObject res;
		try {
			res = b.rpcx("erlang", "process_info", "pa", pid, item);
			if (res instanceof OtpErlangTuple) {
				return ((OtpErlangTuple) res).elementAt(1);
			}
			return null;
		} catch (final Exception e) {
			return null;
		}
	
	}

	public static boolean isErlideProcess(IBackend b, OtpErlangPid pid) {
		boolean res = false;
		try {
			OtpErlangAtom eres = (OtpErlangAtom) b.rpcx("erlide_debug",
					"is_erlide_process", "p", pid);
			res = "true".equals(eres.atomValue());
		} catch (Exception e) {
		}
		return res;
	}

	public static boolean isSystemProcess_(IBackend b, OtpErlangPid pid) {
		boolean res = false;
		try {
			OtpErlangAtom eres = (OtpErlangAtom) b.rpcx("pman_process",
					"is_system_process", "s", pid);
			res = "true".equals(eres.atomValue());
		} catch (Exception e) {
		}
		return res;
	}

}
