package erlang;

import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendEvalResult;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangParseException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideBackend {

	public static boolean init(final Backend backend, final String javaNode) {
		try {
			backend.rpcx("erlide_backend", "init", "a", javaNode);
			return true;
		} catch (final Exception e) {
			ErlLogger.error(e);
			return false;
		}
	}

	public static String format_error(final Backend b,
			final OtpErlangObject object) {
		final OtpErlangTuple err = (OtpErlangTuple) object;
		final OtpErlangAtom mod = (OtpErlangAtom) err.elementAt(1);
		final OtpErlangObject arg = err.elementAt(2);

		String res;
		try {
			RpcResult r = b.rpc(mod.atomValue(), "format_error", "x", arg);
			r = b.rpc("lists", "flatten", "x", r.getValue());
			res = ((OtpErlangString) r.getValue()).stringValue();
		} catch (final Exception e) {
			ErlLogger.error(e);
			res = err.toString();
		}
		return res;
	}

	public static String format(final Backend b, final String fmt,
			final OtpErlangObject... args) {
		try {
			final String r = b
					.rpc("erlide_backend", "format", "slx", fmt, args)
					.toString();
			return r.substring(1, r.length() - 1);
		} catch (final NoBackendException e) {
			return "error";
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
		return "error";
	}

	/**
	 * @param string
	 * @return OtpErlangobject
	 * @throws ErlangParseException
	 */
	public static OtpErlangObject parseTerm(final Backend b, final String string)
			throws ErlangParseException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx("erlide_backend", "parse_term", "s", string);
		} catch (final Exception e) {
			throw new ErlangParseException("Could not parse term \"" + string
					+ "\"");
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;
		if (Util.isOk(t1)) {
			return t1.elementAt(1);
		}
		throw new ErlangParseException("Could not parse term \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	/**
	 * @param string
	 * @return
	 * @throws BackendException
	 */
	public static OtpErlangObject scanString(final Backend b,
			final String string) throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx("erlide_backend", "scan_string", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not tokenize string \"" + string
					+ "\": " + e.getMessage());
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;
		if (Util.isOk(t1)) {
			return t1.elementAt(1);
		}
		throw new BackendException("Could not tokenize string \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	/**
	 * @param string
	 * @return
	 * @throws BackendException
	 */
	public static OtpErlangObject parseString(final Backend b,
			final String string) throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx("erlide_backend", "parse_string", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": " + e.getMessage());
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;
		if (Util.isOk(t1)) {
			return t1.elementAt(1);
		}
		throw new BackendException("Could not parse string \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	public static String prettyPrint(final Backend b, final String text)
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx("erlide_backend", "pretty_print", "s", text + ".");
		} catch (final Exception e) {
			throw new BackendException("Could not parse string \"" + text
					+ "\": " + e.getMessage());
		}
		return ((OtpErlangString) r1).stringValue();
	}

	/**
	 * @param scratch
	 * @param bindings
	 * @return
	 */
	public static BackendEvalResult eval(final Backend b, final String string,
			final OtpErlangObject bindings) {
		final BackendEvalResult result = new BackendEvalResult();
		OtpErlangObject r1;
		try {
			// ErlLogger.debug("eval %s %s", string, bindings);
			if (bindings == null) {
				r1 = b.rpcx("erlide_backend", "eval", "s", string);
			} else {
				r1 = b.rpcx("erlide_backend", "eval", "sx", string, bindings);
			}
			// value may be something else if exception is thrown...
			final OtpErlangTuple t = (OtpErlangTuple) r1;
			final boolean ok = !"error".equals(((OtpErlangAtom) t.elementAt(0))
					.atomValue());
			if (ok) {
				result.setValue(t.elementAt(1), t.elementAt(2));
			} else {
				result.setError(t.elementAt(1));
			}
		} catch (final Exception e) {
			result.setError("rpc failed");
		}
		return result;
	}

	public static void generateRpcStub(final Backend b, final String s) {
		// try {
		// final RpcResult r = b.rpc(ERL_BACKEND, "compile_string", "s", s);
		// if (!r.isOk()) {
		// ErlLogger.debug("rpcstub::" + r.toString());
		// }
		// } catch (final Exception e) {
		// ErlLogger.debug(e);
		// }
	}

	public static boolean loadBeam(final Backend backend,
			final String moduleName, final OtpErlangBinary bin) {
		OtpErlangObject r = null;
		try {
			r = backend.rpcx("code", "is_sticky", "a", moduleName);
			// TODO handle sticky directories
			if (!((OtpErlangAtom) r).booleanValue()) {
				r = backend.rpcx("code", "load_binary", "asb", moduleName,
						moduleName + ".erl", bin);
			} else {
				ErlLogger.warn("sticky:: %s", moduleName);
				r = null;
			}
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}
		if (r != null) {
			final OtpErlangTuple t = (OtpErlangTuple) r;
			if (((OtpErlangAtom) t.elementAt(0)).atomValue()
					.compareTo("module") == 0) {
				return true;
			}
			// code couldn't be loaded
			// maybe here we should throw exception?
			return false;
		}
		// binary couldn't be extracted
		return false;
	}

	public static OtpErlangObject concreteSyntax(final Backend b,
			final OtpErlangObject val) throws BackendException, RpcException {
		try {
			return b.rpcx("erlide_syntax", "concrete", "x", val);
		} catch (final NoBackendException e) {
			return null;
		}
	}

	public static String getScriptId(final Backend b)
			throws ErlangRpcException, BackendException, RpcException {
		OtpErlangObject r;
		r = b.rpcx("init", "script_id", "");
		if (r instanceof OtpErlangTuple) {
			final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
			if (rr instanceof OtpErlangString) {
				return ((OtpErlangString) rr).stringValue();
			}
		}
		return "";
	}

	public static String prettyPrint(final Backend b, final OtpErlangObject e)
			throws ErlangRpcException, BackendException, RpcException {
		OtpErlangObject p = b.rpcx("erlide_pp", "expr", "x", e);
		p = b.rpcx("lists", "flatten", null, p);
		return ((OtpErlangString) p).stringValue();
	}

	public static OtpErlangObject convertErrors(final Backend b,
			final String lines) throws ErlangRpcException, BackendException,
			RpcException {
		OtpErlangObject res;
		res = b.rpcx("erlide_erlcerrors", "convert_erlc_errors", "s", lines);
		return res;
	}

	public static void startTracer(final Backend b, final OtpErlangPid tracer) {
		try {
			ErlLogger.debug("Start tracer to %s", tracer);
			b.rpcx("erlide_backend", "start_tracer", "ps", tracer);
		} catch (final RpcException e) {
		} catch (final BackendException e) {
		}
	}

	public static void startTracer(final Backend b, final String logname) {
		try {
			ErlLogger.debug("Start tracer to %s", logname);
			b.rpcx("erlide_backend", "start_tracer", "s", logname);
		} catch (final RpcException e) {
		} catch (final BackendException e) {
		}
	}

}
