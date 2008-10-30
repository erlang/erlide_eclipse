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

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideScanner {

	public static void initialScan(final String module,
			final String moduleFileName, final String initialText) {
		ErlLogger.debug("initialScan " + module + " init len"
				+ initialText.length());
		final String stateDir = ErlangPlugin.getDefault().getStateLocation()
				.toString();
		try {
			BackendManager.getDefault().getIdeBackend().rpc("erlide_scanner",
					"initialScan", "asss", module, moduleFileName, initialText,
					stateDir);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void destroy(final String module) {
		try {
			BackendManager.getDefault().getIdeBackend().rpcx("erlide_scanner",
					"destroy", "a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	@SuppressWarnings("boxing")
	public static ErlToken getTokenAt(final String module, final int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend()
					.rpcx("erlide_scanner", "do_getTokenAt", "ai", module,
							offset + 1);
		} catch (final Exception e) {
			ErlLogger.debug(e);
			return null;
		}
		if (r1 == null) {
			return null;
		}
		ErlLogger.debug("getTokenAt got %s", r1);
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (Util.isOk(t1)) {
			final OtpErlangTuple tt = (OtpErlangTuple) t1.elementAt(1);
			return new ErlToken(tt, 0);
		}
		return null;
	}

	// @SuppressWarnings("boxing")
	// public static ErlToken[] getTokensAround(String module, int offset) {
	// OtpErlangObject r1 = null;
	// try {
	// r1 = BackendManager.getDefault().getIdeBackend().rpcx(
	// "erlide_scanner", "do_getTokensAround", "ai", module,
	// offset + 1);
	// } catch (final Exception e) {
	// e.printStackTrace();
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
	// final ErlToken[] result = new ErlToken[tt.arity()];
	// for (int i = 0; i < tt.arity(); i++) {
	// result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
	// }
	//
	// }
	// return null;
	// }

	// public static ErlToken[] getTokens(String module) {
	// OtpErlangObject r1 = null;
	// try {
	// r1 = BackendManager.getDefault().getIdeBackend().rpcx(
	// "erlide_scanner", "do_getTokens", "a", module);
	// } catch (final Exception e) {
	// e.printStackTrace();
	// return null;
	// }
	// if (r1 == null) {
	// return null;
	// }
	// final OtpErlangTuple t1 = (OtpErlangTuple) r1;
	//
	// if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
	// final OtpErlangList tt = (OtpErlangList) t1.elementAt(1);
	//
	// final ErlToken[] result = new ErlToken[tt.arity()];
	// for (int i = 0; i < tt.arity(); i++) {
	// result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
	// }
	// return result;
	// }
	// return null;
	// }

	// @SuppressWarnings("boxing")
	// public static TokenWindow getTokenWindow(String module, int offset,
	// int window) {
	// OtpErlangObject r1 = null;
	// try {
	// r1 = BackendManager.getDefault().getIdeBackend().rpcx(
	// "erlide_scanner", "do_getTokenWindow", "aii", module,
	// offset, window);
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
	// final ErlToken[] result = new ErlToken[tt.arity()];
	// for (int i = 0; i < tt.arity(); i++) {
	// result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
	// }
	// return new TokenWindow(tt, window);
	//
	// }
	// return null;
	// }

	@SuppressWarnings("boxing")
	public static void replaceText(final String module, final int offset,
			final int removeLength, final String newText) {
		ErlLogger.debug("replaceText " + module + " (" + offset + ":"
				+ removeLength + ":" + newText.length() + ")");
		try {
			final OtpErlangObject r = BackendManager.getDefault()
					.getIdeBackend().rpcx("erlide_scanner", "replaceText",
							"aiis", module, offset + 1, removeLength, newText);

			if (r instanceof OtpErlangTuple) {
				ErlLogger.error("GOT::" + r.toString());
			} else {
				ErlLogger.debug("repl -> " + r.toString());
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
	public static List<ErlToken> lightScanString(final String string,
			final int offset) throws BackendException {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(
					"erlide_scan", "string", "s", string);
		} catch (final Exception e) {
			throw new BackendException("Could not parse string \"" + string
					+ "\": " + e.getMessage());
		}
		if (r1 == null) {
			return null;
		}

		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		List<ErlToken> toks = null;
		if (Util.isOk(t1)) {
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

}
