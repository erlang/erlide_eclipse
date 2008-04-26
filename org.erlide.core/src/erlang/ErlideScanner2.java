package erlang;

import org.erlide.basiccore.ErlLogger;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlToken;
import org.erlide.core.erlang.TokenWindow;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.NoBackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlideScanner2 {
	private final static String MODULE = "erlide_scanner2";

	public static void initialScan(String module, String moduleFileName,
			String initialText) {
		final String stateDir = ErlangPlugin.getDefault().getStateLocation()
				.toString();
		try {
			BackendManager.getDefault().getIdeBackend().rpc(MODULE,
					"initialScan", "asss", module, moduleFileName, initialText,
					stateDir);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	public static void destroy(String module) {
		try {
			BackendManager.getDefault().getIdeBackend().rpcx(MODULE, "destroy",
					"a", module);
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
	}

	@SuppressWarnings("boxing")
	public static ErlToken getTokenAt(String module, int offset) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(MODULE,
					"getTokenAt", "ai", module, offset + 1);
		} catch (final Exception e) {
			// e.printStackTrace();
			return null;
		}
		if (r1 == null) {
			return null;
		}
		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			final OtpErlangTuple tt = (OtpErlangTuple) t1.elementAt(1);
			return new ErlToken(tt, 0);
		}
		return null;
	}

	@SuppressWarnings("boxing")
	public static TokenWindow getTokenWindow(String module, int offset,
			int window) {
		OtpErlangObject r1 = null;
		try {
			r1 = BackendManager.getDefault().getIdeBackend().rpcx(MODULE,
					"do_getTokenWindow", "aiii", module, offset + 1, window,
					window);
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (final Exception e) {
			ErlLogger.warn(e);
			return null;
		}
		if (r1 == null) {
			return null;
		}

		final OtpErlangTuple t1 = (OtpErlangTuple) r1;

		if (((OtpErlangAtom) t1.elementAt(0)).atomValue().compareTo("ok") == 0) {
			final OtpErlangList tt = (OtpErlangList) t1.elementAt(1);

			final ErlToken[] result = new ErlToken[tt.arity()];
			for (int i = 0; i < tt.arity(); i++) {
				result[i] = new ErlToken((OtpErlangTuple) tt.elementAt(i), 0);
			}
			return new TokenWindow(tt, window);

		}
		return null;
	}

	public static void replaceText(String module, int offset, int removeLength,
			String newText) {
		try {
			final OtpErlangObject r = BackendManager.getDefault()
					.getIdeBackend().rpcx(MODULE, "replaceText", "aiis",
							module, offset + 1, removeLength, newText);
			if (r instanceof OtpErlangTuple) {
				ErlLogger.error("GOT::" + r.toString());
			}
		} catch (final NoBackendException e) {
			ErlLogger.debug(e);
		} catch (final RpcException e) {
		} catch (final BackendException e) {
		}
	}

}
