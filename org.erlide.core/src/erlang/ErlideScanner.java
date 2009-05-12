package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.util.Util;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideScanner {
	private static final String ERLIDE_SCANNER = "erlide_scanner";

	public static void initialScan(final String module,
			final String moduleFileName, final String initialText,
			final String erlidePath) {
		final String stateDir = ErlangPlugin.getDefault().getStateLocation()
				.toString();
		try {
			final OtpErlangObject res = ErlangCore.getBackendManager()
					.getIdeBackend().call(ERLIDE_SCANNER, "initialScan",
							"assss", module, moduleFileName, initialText,
							stateDir, erlidePath);
			ErlLogger.debug("initialScan " + res);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void destroy(final String module) {
		try {
			ErlangCore.getBackendManager().getIdeBackend().call(ERLIDE_SCANNER,
					"destroy", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	@SuppressWarnings("boxing")
	public static ErlToken getTokenAt(final String module, final int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = ErlangCore.getBackendManager().getIdeBackend().call(
					ERLIDE_SCANNER, "getTokenAt", "ai", module, offset);
			// ErlLogger.debug("getTokenAt -> " + r1);
		} catch (final Exception e) {
			// e.printStackTrace();
			return null;
		}
		if (r1 == null || !(r1 instanceof OtpErlangTuple)) {
			return null;
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;
		if (Util.isOk(t1)) {
			final OtpErlangObject ot = t1.elementAt(1);
			if (ot instanceof OtpErlangTuple) {
				final OtpErlangTuple tt = (OtpErlangTuple) ot;
				return new ErlToken(tt, 0);
			}
		}
		return null;
	}

	@SuppressWarnings("boxing")
	public static void replaceText(final String module, final int offset,
			final int removeLength, final String newText) {
		try {
			final int newTextLen = newText == null ? 0 : newText.length();
			ErlLogger.debug("replaceText %s %d %d <length %d>", module, offset,
					removeLength, newTextLen);
			// ErlLogger.debug("replaceText %s %d %d \"%s\"", module, offset,
			// removeLength, newText);
			final OtpErlangObject r = ErlangCore.getBackendManager()
					.getIdeBackend().call(ERLIDE_SCANNER, "replaceText",
							"aiis", module, offset, removeLength, newText);
			if (r instanceof OtpErlangTuple) {
				ErlLogger.error("GOT::" + r.toString());
			}
		} catch (final BackendException e) {
			ErlLogger.debug(e);
		}
	}

	/**
	 * @param string
	 * @param offset
	 * @return
	 * @throws BackendException
	 */
	public static List<ErlToken> lightScanString(final String string,
			final int offset) throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = ErlangCore.getBackendManager().getIdeBackend().call(
					"erlide_scanner", "light_scan_string", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": " + e.getMessage());
		}
		if (r1 == null) {
			return null;
		}

		if (!(r1 instanceof OtpErlangTuple)) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": weird return value " + r1);
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		List<ErlToken> toks = null;
		if (!(t1.elementAt(0) instanceof OtpErlangAtom)) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": funny return value" + t1);
		}
		if (Util.isOk(t1)) {
			if (t1.elementAt(1) instanceof OtpErlangList) {
				final OtpErlangList l = (OtpErlangList) t1.elementAt(1);
				if (l != null) {
					toks = new ArrayList<ErlToken>(l.arity() + 1);
					for (final OtpErlangObject o : l) {
						final OtpErlangTuple t = (OtpErlangTuple) o;
						final ErlToken tk = new ErlToken(t, 0);
						tk.fixOffset(offset);
						toks.add(tk);
					}
					return toks;
				}
			}
		}
		throw new BackendException("Could not parse string \"" + string
				+ "\": " + t1.elementAt(1).toString());
	}

	public static String checkAll(final String module, final String text) {
		try {
			final OtpErlangObject o = ErlangCore.getBackendManager()
					.getIdeBackend().call(ERLIDE_SCANNER, "check_all", "as",
							module, text);
			return o.toString();
		} catch (final BackendException e) {
			return "";
		}

	}

	@SuppressWarnings("boxing")
	public static void notifyChange(final String module, final int offset,
			final int length, final String text) {
		try {
			final OtpErlangObject msg = ErlUtils.format(
					"{change, ~a, ~i,  ~i, ~s}", module, offset, length, text);
			ErlangCore.getBackendManager().getIdeBackend().send(
					"erlide_scanner_listener", msg);
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}
	}

	public static void notifyNew(final String module) {
		try {
			final OtpErlangObject msg = ErlUtils.format("{new, ~a}", module);
			ErlangCore.getBackendManager().getIdeBackend().send(
					"erlide_scanner_listener", msg);
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}
	}

}
