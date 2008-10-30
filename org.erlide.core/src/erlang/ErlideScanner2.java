package erlang;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideScanner2 {
	private final static String ERLIDE_SCANNER2 = "erlide_scanner2";

	public static void initialScan(final String module,
			final String moduleFileName, final String initialText,
			final String erlidePath) {
		final String stateDir = ErlangPlugin.getDefault().getStateLocation()
				.toString();
		try {
			BackendManager.getDefault().getIdeBackend().rpc(ERLIDE_SCANNER2,
					"initialScan", "assss", module, moduleFileName,
					initialText, stateDir, erlidePath);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void destroy(final String module) {
		try {
			BackendManager.getDefault().getIdeBackend().rpcx(ERLIDE_SCANNER2,
					"destroy", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	@SuppressWarnings("boxing")
	public static ErlToken getTokenAt(final String module, final int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					ERLIDE_SCANNER2, "getTokenAt", "ai", module, offset);
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

	// @SuppressWarnings("boxing")
	// public static TokenWindow getTokenWindow(String module, int offset,
	// int window) {
	// OtpErlangObject r1 = null;
	// try {
	// r1 = BackendManager.getDefault().getIdeBackend().rpcx(MODULE,
	// "do_getTokenWindow", "aiii", module, offset + 1, window,
	// window);
	// } catch (final NoBackendException e) {
	// ErlLogger.debug(e);
	// } catch (final Exception e) {
	// ErlLogger.warn(e);
	// return null;
	// }
	// if (r1 == null) {
	// return null;
	// }
	//
	// final OtpErlangTuple t1 = (OtpErlangTuple) r1;
	//
	// if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
	// final OtpErlangList tt = (OtpErlangList) t1.elementAt(1);
	//
	// // final ErlToken[] result = new ErlToken[tt.arity()];
	// // for (int i = 0; i < tt.arity(); i++) {
	// // result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
	// // }
	// return new TokenWindow(tt, window);
	//
	// }
	// return null;
	// }

	@SuppressWarnings("boxing")
	public static void replaceText(final String module, final int offset,
			final int removeLength, final String newText) {
		try {
			ErlLogger.info("replaceText %d %d \"%d\"", offset, removeLength,
					newText.length());
			final OtpErlangObject r = BackendManager.getDefault()
					.getIdeBackend().rpcx(ERLIDE_SCANNER2, "replaceText",
							"aiis", module, offset, removeLength, newText);
			if (r instanceof OtpErlangTuple) {
				ErlLogger.error("GOT::" + r.toString());
			}
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (final RpcException e) {
		} catch (final BackendException e) {
		}
	}

	/**
	 * @param string
	 * @param offset
	 * @return
	 * @throws BackendException
	 */
	private static final OtpErlangTuple TUPLE00 = new OtpErlangTuple(
			new OtpErlangLong(0), new OtpErlangLong(0));

	public static List<ErlToken> lightScanString(final String string,
			final int offset) throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scan", "string", "sx", string, TUPLE00);
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
		if (Util.isOk(t1.elementAt(0))) {
			if (t1.elementAt(1) instanceof OtpErlangList) {
				final OtpErlangList l = (OtpErlangList) t1.elementAt(1);
				if (l != null) {
					toks = new ArrayList<ErlToken>(l.arity() + 1);
					for (int i = 0; i < l.arity(); i++) {
						final OtpErlangTuple e = (OtpErlangTuple) l
								.elementAt(i);
						final ErlToken tk = new ErlToken(e);
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

	public static String checkAll(final String module, final String text)
			throws BackendException {
		try {
			final OtpErlangObject o = BackendManager.getDefault()
					.getIdeBackend().rpcx("erlide_scanner2", "check_all", "as",
							module, text);
			return o.toString();
		} catch (final RpcException e) {
			return "";
		}

	}

}
