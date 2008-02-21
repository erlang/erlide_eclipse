package erlang;

import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendEvalResult;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangParseException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideBackend {

	public static OtpErlangObject execute(IBackend backend, String fun,
			OtpErlangObject... args) throws ErlangRpcException, RpcException {
		return backend.rpc("erlide_backend", "execute", "sx", fun,
				new OtpErlangList(args)).getValue();
	}

	public static String format_error(final OtpErlangObject object) {
		final OtpErlangTuple err = (OtpErlangTuple) object;
		final OtpErlangAtom mod = (OtpErlangAtom) err.elementAt(1);
		final OtpErlangObject arg = err.elementAt(2);

		String res;
		try {
			RpcResult r = BackendManager.getDefault().getIdeBackend().rpc(
					mod.atomValue(), "format_error", "x", arg);
			r = BackendManager.getDefault().getIdeBackend().rpc("lists",
					"flatten", "x", r.getValue());
			res = ((OtpErlangString) r.getValue()).stringValue();
		} catch (final Exception e) {
			e.printStackTrace();
			res = err.toString();
		}
		return res;
	}

	public static String format(IBackend b, String fmt, OtpErlangObject... args) {
		try {
			final String r = b.rpc(IBackend.ERL_BACKEND, "format", "sx", fmt,
					new OtpErlangList(args)).toString();
			return r.substring(1, r.length() - 1);
		} catch (final Exception e) {
			e.printStackTrace();
		}
		return "error";
	}

	/**
	 * @param string
	 * @return OtpErlangobject
	 * @throws ErlangParseException
	 */
	public static OtpErlangObject parseTerm(IBackend b, String string)
			throws ErlangParseException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx(IBackend.ERL_BACKEND, "parse_term", "s", string);
		} catch (final Exception e) {
			throw new ErlangParseException("Could not parse term \"" + string
					+ "\"");
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
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
	public static OtpErlangObject scanString(IBackend b, String string)
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx(IBackend.ERL_BACKEND, "scan_string", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not tokenize string \"" + string
					+ "\": " + e.getMessage());
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
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
	public static OtpErlangObject parseString(IBackend b, String string)
			throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = b.rpcx(IBackend.ERL_BACKEND, "parse_string", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": " + e.getMessage());
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			return t1.elementAt(1);
		}
		throw new BackendException("Could not parse string \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	public static String prettyPrint(IBackend b, String text)
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
	public static BackendEvalResult eval(IBackend b, String string,
			OtpErlangObject bindings) {
		final BackendEvalResult result = new BackendEvalResult();
		OtpErlangObject r1;
		try {
			// ErlLogger.debug("eval %s %s", string, bindings);
			if (bindings == null) {
				r1 = b.rpcx(IBackend.ERL_BACKEND, "eval", "s", string);
			} else {
				r1 = b.rpcx(IBackend.ERL_BACKEND, "eval", "sx", string,
						bindings);
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

	public static void generateRpcStub(IBackend b, String s) {
		try {
			RpcResult r = b.rpc("erlide_backend", "compile_string", "s", s);
			if (!r.isOk()) {
				ErlLogger.debug("rpcstub::" + r.toString());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void init(IBackend backend, String node) {
		try {
			backend.rpc("erlide_backend", "init", "a", node);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static boolean loadBeam(IBackend backend, String moduleName,
			final OtpErlangBinary bin) {
		OtpErlangObject r = null;
		try {
			r = backend.rpcx("code", "is_sticky", "a", moduleName);
			if (!((OtpErlangAtom) r).booleanValue()
					|| !BackendManager.isDeveloper()) {
				r = backend.rpcx("code", "load_binary", "asb", moduleName,
						moduleName + ".erl", bin);
				if (BackendManager.isDeveloper()) {
					backend.rpc("code", "stick_mod", "a", moduleName);
				}
			} else {
				r = null;
			}
		} catch (final Exception e) {
			e.printStackTrace();
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

}
