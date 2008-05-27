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
	public static OtpErlangList getProcesses(final IBackend fBackend,
			final boolean fShowSystemProcesses,
			final boolean fShowErlideProcesses) {
		OtpErlangList procs = null;
		try {
			procs = (OtpErlangList) BackendUtil.ok(fBackend.rpcx(
					"erlide_debug", "processes", "oo", fShowSystemProcesses,
					fShowErlideProcesses));
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return procs;
	}

	public static OtpErlangPid startDebug(final IBackend b, final String mod,
			final String func) throws ErlangRpcException, BackendException,
			RpcException {
		OtpErlangObject res;
		res = b.rpcx("erlide_debug", "start_debug", "aa", mod, func);
		final OtpErlangPid pid = (OtpErlangPid) BackendUtil.ok(res);
		return pid;
	}

	public static OtpErlangObject getProcessInfo(final IBackend b,
			final OtpErlangPid pid, final String item) {
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

	public static boolean isErlideProcess(final IBackend b,
			final OtpErlangPid pid) {
		boolean res = false;
		try {
			final OtpErlangAtom eres = (OtpErlangAtom) b.rpcx("erlide_debug",
					"is_erlide_process", "p", pid);
			res = "true".equals(eres.atomValue());
		} catch (final Exception e) {
		}
		return res;
	}

	public static boolean isSystemProcess(final IBackend b,
			final OtpErlangPid pid) {
		boolean res = false;
		try {
			final OtpErlangAtom eres = (OtpErlangAtom) b.rpcx("pman_process",
					"is_system_process", "s", pid);
			res = "true".equals(eres.atomValue());
		} catch (final Exception e) {
		}
		return res;
	}

}
