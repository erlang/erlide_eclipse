package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.rpc.generator.RpcStubGenerator;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.TypeConverter;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.BackendEvalResult;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideBackend {

	public static void reload(final Backend backend) {
		try {
			final OtpErlangList loaded = (OtpErlangList) backend.call("code",
					"all_loaded", "");
			final List<OtpErlangAtom> mine = new ArrayList<OtpErlangAtom>();
			for (final OtpErlangObject elem : loaded) {
				final OtpErlangTuple t = (OtpErlangTuple) elem;
				final OtpErlangAtom mod = (OtpErlangAtom) t.elementAt(0);
				if (mod.atomValue().startsWith("erlide_")) {
					// ErlLogger.debug(">>> HAD " + mod + "   " +
					// t.elementAt(1));
					mine.add(mod);
				}
			}
			for (final OtpErlangAtom mod : mine) {
				// ErlLogger.debug(">>> reload " + mod);
				backend.call("c", "l", "x", mod);
			}
		} catch (final Exception e) {
			ErlLogger.error(e);
		}
	}

	public static boolean init(final Backend backend, final OtpErlangPid jRex) {
		try {
			// reload(backend);
			backend.call("erlide_backend", "init", "p", jRex);
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
			OtpErlangObject r = b.call(mod.atomValue(), "format_error", "x",
					arg);
			r = b.call("lists", "flatten", "x", r);
			res = ((OtpErlangString) r).stringValue();
		} catch (final Exception e) {
			ErlLogger.error(e);
			res = err.toString();
		}
		return res;
	}

	public static String format(final Backend b, final String fmt,
			final OtpErlangObject... args) {
		try {
			final String r = b.call("erlide_backend", "format", "slx", fmt,
					args).toString();
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
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.call("erlide_backend", "parse_term", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not parse term \"" + string
					+ "\"");
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;
		if (Util.isOk(t1)) {
			return t1.elementAt(1);
		}
		throw new BackendException("Could not parse term \"" + string + "\": "
				+ t1.elementAt(1).toString());
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
			r1 = b.call("erlide_backend", "scan_string", "s", string);
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
	 */
	public static OtpErlangObject parseString(final Backend b,
			final String string) throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.call("erlide_backend", "parse_string", "s", string);
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
			r1 = b.call("erlide_backend", "pretty_print", "s", text + ".");
		} catch (final Exception e) {
			throw new BackendException("Could not parse string \"" + text
					+ "\": " + e.getMessage());
		}
		return ((OtpErlangString) r1).stringValue();
	}

	public static BackendEvalResult eval(final Backend b, final String string,
			final OtpErlangObject bindings) {
		final BackendEvalResult result = new BackendEvalResult();
		try {
			OtpErlangObject r1;
			// ErlLogger.debug("eval %s %s", string, bindings);
			if (bindings == null) {
				r1 = b.call("erlide_backend", "eval", "s", string);
			} else {
				r1 = b.call("erlide_backend", "eval", "sx", string, bindings);
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
			r = backend.call("code", "is_sticky", "a", moduleName);
			// TODO handle sticky directories
			if (!((OtpErlangAtom) r).booleanValue()) {
				r = backend.call("code", "load_binary", "asb", moduleName,
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
			final OtpErlangObject val) {
		try {
			return b.call("erlide_syntax", "concrete", "x", val);
		} catch (final BackendException e) {
			return null;
		}
	}

	public static String getScriptId(final Backend b) throws BackendException {
		OtpErlangObject r;
		r = b.call("init", "script_id", "");
		if (r instanceof OtpErlangTuple) {
			final OtpErlangObject rr = ((OtpErlangTuple) r).elementAt(1);
			if (rr instanceof OtpErlangString) {
				return ((OtpErlangString) rr).stringValue();
			}
		}
		return "";
	}

	public static String prettyPrint(final Backend b, final OtpErlangObject e)
			throws BackendException {
		OtpErlangObject p = b.call("erlide_pp", "expr", "x", e);
		p = b.call("lists", "flatten", "x", p);
		return ((OtpErlangString) p).stringValue();
	}

	public static OtpErlangObject convertErrors(final Backend b,
			final String lines) throws BackendException {
		OtpErlangObject res;
		res = b.call("erlide_erlcerrors", "convert_erlc_errors", "s", lines);
		return res;
	}

	public static void startTracer(final Backend b, final OtpErlangPid tracer) {
		try {
			ErlLogger.debug("Start tracer to %s", tracer);
			b.call("erlide_backend", "start_tracer", "ps", tracer);
		} catch (final BackendException e) {
		}
	}

	public static void startTracer(final Backend b, final String logname) {
		try {
			ErlLogger.debug("Start tracer to %s", logname);
			b.call("erlide_backend", "start_tracer", "s", logname);
		} catch (final BackendException e) {
		}
	}

	public static void generateRpcStub(final String className,
			final boolean onlyDeclared, final Backend b) {
		generateRpcStub(TypeConverter.getClassByName(className), onlyDeclared,
				b);
	}

	public static void generateRpcStub(final Class<?> cls,
			final boolean onlyDeclared, final Backend b) {
		final String s = RpcStubGenerator.generate(cls, onlyDeclared);
		generateRpcStub(b, s);
	}

}
