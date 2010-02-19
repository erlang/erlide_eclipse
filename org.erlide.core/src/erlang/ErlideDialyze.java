package erlang;

import java.util.Collection;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlideDialyze {

	public static OtpErlangObject dialyze(final Backend backend,
			final Collection<String> files, final String plt) {
		try {
			final OtpErlangObject result = backend.call("erlide_dialyze",
					"dialyze", "sls", files, plt);
			return result;
		} catch (final Exception e) {
			ErlLogger.debug(e);
		}
		return null;
	}

}
