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
	public static OtpErlangList getProcesses(final IBackend backend,
			final boolean showSystemProcesses, final boolean showErlideProcesses) {
		OtpErlangList procs = null;
		try {
			procs = (OtpErlangList) BackendUtil.ok(backend
					.rpcx("erlide_debug", "processes", "oo",
							showSystemProcesses, showErlideProcesses));
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return procs;
	}

	public static OtpErlangPid startDebug(final IBackend backend) {
		OtpErlangObject res = null;
		try {
			res = backend.rpcx("erlide_debug", "start_debug", "");
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

	/**
	 * Sent upon attach of process (should we move this to erlang, erlide_debug?
	 * maybe we should have an erlang process subscribing, and even filtering
	 * events to us)
	 * 
	 * @param backend
	 *            backend
	 * @param pid
	 *            pid of attached process
	 * @param jpid
	 *            java pseudo-pid
	 * @return pid of meta process
	 */
	public static OtpErlangPid attached(final IBackend backend,
			final OtpErlangPid pid, final OtpErlangPid jpid) {
		OtpErlangObject res = null;
		try {
			res = backend.rpcx("erlide_debug", "attached", "xx", pid, jpid);
			final OtpErlangTuple t = (OtpErlangTuple) res;
			final OtpErlangPid meta = (OtpErlangPid) t.elementAt(1);
			return meta;
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public static OtpErlangObject getProcessInfo(final IBackend backend,
			final OtpErlangPid pid, final String item) {
		OtpErlangObject res;
		try {
			res = backend.rpcx("erlang", "process_info", "pa", pid, item);
			if (res instanceof OtpErlangTuple) {
				return ((OtpErlangTuple) res).elementAt(1);
			}
			return null;
		} catch (final Exception e) {
			return null;
		}

	}

	public static boolean isErlideProcess(final IBackend backend,
			final OtpErlangPid pid) {
		boolean res = false;
		try {
			final OtpErlangAtom eres = (OtpErlangAtom) backend.rpcx(
					"erlide_debug", "is_erlide_process", "p", pid);
			res = "true".equals(eres.atomValue());
		} catch (final Exception e) {
		}
		return res;
	}

	public static boolean interpret(final IBackend backend, final String module) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"interpret", "s", module);
			if (res instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangObject o = t.elementAt(0);
				if (o instanceof OtpErlangAtom) {
					final OtpErlangAtom moduleAtom = (OtpErlangAtom) o;
					return moduleAtom.atomValue().equals("module");
				}
			}
			final OtpErlangAtom ok = (OtpErlangAtom) res;
			return ok.atomValue().equals("ok");
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return false;
	}

	public static boolean isSystemProcess(final IBackend backend,
			final OtpErlangPid pid) {
		boolean res = false;
		try {
			final OtpErlangAtom eres = (OtpErlangAtom) backend.rpcx(
					"pman_process", "is_system_process", "s", pid);
			res = "true".equals(eres.atomValue());
		} catch (final Exception e) {
		}
		return res;
	}

	public static void addLineBreakpoint(final IBackend backend,
			final String module, final int line) {
		try {
			backend.rpcx("erlide_debug", "line_breakpoint", "si", module, line);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void sendStarted(final IBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "send_started", "x", meta);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void resume(final IBackend backend, final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "resume", "x", meta);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void suspend(final IBackend backend, final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "suspend", "x", meta);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static OtpErlangList getBindings(final IBackend backend,
			final OtpErlangPid meta) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"bindings", "x", meta);
			return (OtpErlangList) res;
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

	public static void stepOver(final IBackend backend, final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "step_over", "x", meta);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void stepReturn(final IBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "step_return", "x", meta);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void stepInto(final IBackend backend, final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "step_into", "x", meta);
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static OtpErlangTuple getAllStackframes(final IBackend backend,
			final OtpErlangPid meta) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"all_stack_frames", "x", meta);
			if (res instanceof OtpErlangTuple) {
				return (OtpErlangTuple) res;
			}
		} catch (final RpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}
}