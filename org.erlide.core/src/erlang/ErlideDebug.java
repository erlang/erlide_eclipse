package erlang;

import java.util.List;

import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.ExecutionBackend;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.debug.ErlDebugConstants;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideDebug {

	@SuppressWarnings("boxing")
	public static OtpErlangList getProcesses(final ExecutionBackend backend,
			final boolean showSystemProcesses, final boolean showErlideProcesses) {
		OtpErlangList procs = null;
		try {
			procs = (OtpErlangList) BackendUtil.ok(backend
					.rpcx("erlide_debug", "processes", "oo",
							showSystemProcesses, showErlideProcesses));
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return procs;
	}

	@SuppressWarnings("boxing")
	public static OtpErlangPid startDebug(final IBackend backend,
			final int debugFlags) {
		OtpErlangObject res = null;
		try {
			res = backend.rpcx("erlide_debug", "start_debug", "i", debugFlags);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
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
			if (res instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangPid meta = (OtpErlangPid) t.elementAt(1);
				return meta;
			} else {
				return null;
			}
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
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

	@SuppressWarnings("boxing")
	public static boolean interpret(final IBackend backend,
			final String module, final boolean distributed,
			final boolean interpret) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"interpret", "soo", module, distributed, interpret);
			if (res instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangObject o = t.elementAt(0);
				if (o instanceof OtpErlangAtom) {
					final OtpErlangAtom moduleAtom = (OtpErlangAtom) o;
					return moduleAtom.atomValue().equals("module");
				}
			}
			return Util.isOk(res);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
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

	@SuppressWarnings("boxing")
	public static void addDeleteLineBreakpoint(final IBackend backend,
			final String module, final int line, final int action) {
		try {
			final String a = action == ErlDebugConstants.REQUEST_INSTALL ? "add"
					: "delete";
			backend.rpcx("erlide_debug", "line_breakpoint", "sia", module,
					line, a);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static void sendStarted(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "send_started", "x", meta);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static void resume(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "resume", "x", meta);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static void suspend(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "suspend", "x", meta);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static OtpErlangList getBindings(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"bindings", "x", meta);
			return (OtpErlangList) res;
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return null;
	}

	public static void stepOver(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "step_over", "x", meta);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static void stepReturn(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "step_return", "x", meta);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static void stepInto(final ExecutionBackend backend,
			final OtpErlangPid meta) {
		try {
			backend.rpcx("erlide_debug", "step_into", "x", meta);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
	}

	public static OtpErlangTuple getAllStackframes(
			final ExecutionBackend backend, final OtpErlangPid meta) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"all_stack_frames", "x", meta);
			if (res instanceof OtpErlangTuple) {
				return (OtpErlangTuple) res;
			}
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return null;
	}

	public static OtpErlangObject eval(final ExecutionBackend backend,
			final String expression, final OtpErlangPid meta) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug", "eval",
					"sx", expression, meta);
			if (res instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				if (Util.isOk(t)) {
					return t.elementAt(1);
				}
			}
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return null;
	}

	public static final OtpErlangAtom OK = new OtpErlangAtom("ok");

	@SuppressWarnings("boxing")
	public static String setVariableValue(final ExecutionBackend backend,
			final String name, final String value, final int stackFrameNo,
			final OtpErlangPid meta) {
		try {
			final OtpErlangObject res = backend.rpcx("erlide_debug",
					"set_variable_value", "ssix", name, value,
					stackFrameNo + 1, meta);
			if (res instanceof OtpErlangTuple) {
				final OtpErlangTuple t = (OtpErlangTuple) res;
				final OtpErlangObject o = t.elementAt(1);
				if (o instanceof OtpErlangTuple) {
					final OtpErlangTuple t1 = (OtpErlangTuple) o;
					final OtpErlangObject o10 = t1.elementAt(0);
					final OtpErlangObject o11 = t1.elementAt(1);
					if (o10 instanceof OtpErlangAtom) {
						final OtpErlangAtom e = (OtpErlangAtom) o10;
						if (e.atomValue().equals("error")) {
							if (o11 instanceof OtpErlangAtom) {
								final OtpErlangAtom e11 = (OtpErlangAtom) o11;
								return e11.atomValue();
							} else if (o11 instanceof OtpErlangString) {
								final OtpErlangString s11 = (OtpErlangString) o11;
								return s11.stringValue();
							} else {
								return "error";
							}
						}
					}
				}
			}
			return null;
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return "error";
	}

	public static OtpErlangObject distributeDebuggerCode(
			final ExecutionBackend backend, final List<OtpErlangTuple> modules) {
		try {
			final OtpErlangObject o = backend.rpcx("erlide_debug",
					"distribute_debugger_code", "lx", modules);
			return o;
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		} catch (final BackendException e) {
			ErlLogger.warn(e);
		}
		return null;
	}
}
