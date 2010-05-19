package erlang;

import java.util.Collection;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideDialyze {

	private static final int LONG_TIMEOUT = 100000;

	public static OtpErlangObject dialyze(final Backend backend,
			final Collection<String> files, final String plt,
			final Collection<String> includeDirs, final boolean fromSource) {
		try {
			final OtpErlangObject result = backend.call(LONG_TIMEOUT,
					"erlide_dialyze", "dialyze", "lsslso", files, plt,
					includeDirs, fromSource);
			return result;
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
		return null;
	}

	public static String formatWarning(final Backend backend,
			final OtpErlangObject warning) {
		try {
			final OtpErlangObject result = backend.call("erlide_dialyze",
					"format_warning", "x", warning);
			return Util.stringValue(result);
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return warning.toString();
	}

	public static OtpErlangObject checkPlt(final Backend backend,
			final String plt) {
		try {
			return backend.call("erlide_dialyze", "check_plt", "s", plt);
		} catch (final BackendException e) {
			ErlLogger.debug(e);
		}
		return null;
	}

}
