package erlang;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

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

	public static OtpErlangPid startDebug(final IBackend b,
			final OtpErlangPid jpid) {
		OtpErlangObject res = null;
		try {
			res = b.rpcx("erlide_debug", "start_debug", "x", jpid);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		final OtpErlangPid pid = (OtpErlangPid) res;
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

	public static boolean interpret(final IBackend b, final String module) {
		try {
			final OtpErlangAtom ok = (OtpErlangAtom) b.rpcx("erlide_debug",
					"interpret", "a", module);
			return ok.atomValue().equals("ok");
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return false;
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

	public static void addLineBreakpoint(final IBackend b, final String module,
			final int line) {
		try {
			b.rpcx("erlide_debug", "line_breakpoint", "si", module, line);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
