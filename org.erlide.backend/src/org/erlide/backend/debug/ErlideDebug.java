package org.erlide.backend.debug;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.DebugException;
import org.erlide.backend.BackendPlugin;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.IBackend;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideDebug {

    private static final String ERLIDE_DEBUG = "erlide_debug";

    @SuppressWarnings("boxing")
    public static OtpErlangList getProcesses(final IRpcSite backend,
            final boolean showSystemProcesses, final boolean showErlideProcesses) {
        OtpErlangList procs = null;
        try {
            procs = (OtpErlangList) BackendUtils.ok(backend
                    .call(ERLIDE_DEBUG, "processes", "oo", showSystemProcesses,
                            showErlideProcesses));
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return procs;
    }

    @SuppressWarnings("boxing")
    public static OtpErlangPid startDebug(final IRpcSite backend,
            final int debugFlags) throws DebugException {
        OtpErlangObject res = null;
        try {
            res = backend.call(ERLIDE_DEBUG, "start_debug", "i", debugFlags);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        if (res instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) res;
            final OtpErlangObject o = t.elementAt(1);
            final IStatus s = new Status(IStatus.ERROR,
                    BackendPlugin.PLUGIN_ID, DebugException.REQUEST_FAILED,
                    o.toString(), null);
            throw new DebugException(s);
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
    public static OtpErlangPid attached(final IRpcSite backend,
            final OtpErlangPid pid, final OtpErlangPid jpid) {
        OtpErlangObject res = null;
        try {
            res = backend.call(ERLIDE_DEBUG, "attached", "xx", pid, jpid);
            if (res instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                final OtpErlangPid meta = (OtpErlangPid) t.elementAt(1);
                return meta;
            }
            return null;
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static OtpErlangObject getProcessInfo(final IRpcSite backend,
            final OtpErlangPid pid, final String item) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG,
                    "process_info", "pa", pid, item);
            if (res instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                return t.elementAt(1);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static boolean isErlideProcess(final IRpcSite backend,
            final OtpErlangPid pid) {
        boolean res = false;
        try {
            final OtpErlangAtom eres = (OtpErlangAtom) backend.call(
                    ERLIDE_DEBUG, "is_erlide_process", "p", pid);
            res = Boolean.parseBoolean(eres.atomValue());
        } catch (final Exception e) {
        }
        return res;
    }

    @SuppressWarnings("boxing")
    public static boolean interpret(final IRpcSite backend,
            final String module, final boolean distributed,
            final boolean interpret) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG, "interpret",
                    "soo", module, distributed, interpret);
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
        }
        return false;
    }

    public static boolean isSystemProcess(final IRpcSite backend,
            final OtpErlangPid pid) {
        boolean res = false;
        try {
            final OtpErlangAtom eres = (OtpErlangAtom) backend.call(
                    "pman_process", "is_system_process", "s", pid);
            res = Boolean.parseBoolean(eres.atomValue());
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
            backend.getRpcSite().call(ERLIDE_DEBUG, "line_breakpoint", "sia",
                    module, line, a);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static void sendStarted(final IBackend backend,
            final OtpErlangPid meta) {
        try {
            backend.getRpcSite().call(ERLIDE_DEBUG, "send_started", "x", meta);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static void resume(final IBackend backend, final OtpErlangPid meta) {
        try {
            backend.getRpcSite().call(ERLIDE_DEBUG, "resume", "x", meta);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static void suspend(final IBackend backend, final OtpErlangPid meta) {
        try {
            backend.getRpcSite().call(ERLIDE_DEBUG, "suspend", "x", meta);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static OtpErlangList getBindings(final IBackend backend,
            final OtpErlangPid meta) {
        try {
            final OtpErlangObject res = backend.getRpcSite().call(ERLIDE_DEBUG,
                    "bindings", "x", meta);
            return (OtpErlangList) res;
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static void stepOver(final IBackend backend, final OtpErlangPid meta) {
        try {
            backend.getRpcSite().call(ERLIDE_DEBUG, "step_over", "x", meta);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static void stepReturn(final IBackend backend,
            final OtpErlangPid meta) {
        try {
            backend.getRpcSite().call(ERLIDE_DEBUG, "step_return", "x", meta);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static void stepInto(final IBackend backend, final OtpErlangPid meta) {
        try {
            backend.getRpcSite().call(ERLIDE_DEBUG, "step_into", "x", meta);
        } catch (final RpcTimeoutException e) {
            if (backend.isRunning()) {
                ErlLogger.warn(e);
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
    }

    public static OtpErlangTuple getAllStackframes(final IRpcSite backend,
            final OtpErlangPid meta) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG,
                    "all_stack_frames", "x", meta);
            if (res instanceof OtpErlangTuple) {
                return (OtpErlangTuple) res;
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static List<String> getAllModulesOnStack(final IRpcSite backend,
            final OtpErlangPid meta) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG,
                    "all_modules_on_stack", "x", meta);
            if (res instanceof OtpErlangList) {
                final OtpErlangList modules = (OtpErlangList) res;
                final List<String> result = new ArrayList<String>(
                        modules.arity());
                for (final OtpErlangObject module : modules) {
                    final OtpErlangAtom moduleA = (OtpErlangAtom) module;
                    result.add(moduleA.atomValue());
                }
                return result;
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
            ErlLogger.error(e);
        }
        return null;
    }

    @SuppressWarnings("boxing")
    public static OtpErlangTuple tracing(final IRpcSite backend,
            final boolean trace, final OtpErlangPid meta) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG, "tracing",
                    "ox", trace, meta);
            if (res instanceof OtpErlangTuple) {
                return (OtpErlangTuple) res;
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static OtpErlangObject eval(final IRpcSite backend,
            final String expression, final OtpErlangPid meta) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG, "eval",
                    "sx", expression, meta);
            if (res instanceof OtpErlangTuple) {
                final OtpErlangTuple t = (OtpErlangTuple) res;
                if (Util.isOk(t)) {
                    return t.elementAt(1);
                }
            }
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static final OtpErlangAtom OK = new OtpErlangAtom("ok");

    @SuppressWarnings("boxing")
    public static String setVariableValue(final IRpcSite backend,
            final String name, final String value, final int stackFrameNo,
            final OtpErlangPid meta) {
        try {
            final OtpErlangObject res = backend.call(ERLIDE_DEBUG,
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
        }
        return "error";
    }

    public static OtpErlangObject distributeDebuggerCode(
            final IRpcSite backend, final List<OtpErlangTuple> modules) {
        try {
            final OtpErlangObject o = backend.call(ERLIDE_DEBUG,
                    "distribute_debugger_code", "lx", modules);
            return o;
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }

    public static void unloadDebuggerCode(final IRpcSite backend,
            final List<String> modules) {
        try {
            backend.cast(ERLIDE_DEBUG, "unload_debugger_code", "la", modules);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return;
    }

    public static OtpErlangList nodes(final IRpcSite backend) {
        try {
            final OtpErlangObject o = backend.call(ERLIDE_DEBUG, "nodes", "");
            if (o instanceof OtpErlangList) {
                return (OtpErlangList) o;
            }
        } catch (final RpcException e) {
            ErlLogger.error(e);
        }
        return null;
    }

    public static boolean dropToFrame(final IRpcSite backend,
            final OtpErlangPid metaPid, final int stackFrameNo) {
        try {
            final OtpErlangObject o = backend.call(ERLIDE_DEBUG,
                    "drop_to_frame", "xi", metaPid, stackFrameNo);
            return Util.isOk(o);
        } catch (final RpcException e) {
        }
        return false;
    }
}
