/**
 *
 */
package erlang;

import java.util.List;

import org.erlide.core.erlang.ErlProjectImport;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author jakob
 * 
 */
public class ErlideImport {

	/**
	 * Filter import parameters for newly imported erlang project
	 * 
	 * @param prefix
	 *            TODO
	 * @param importSources
	 * @param prefix
	 * 
	 * @return
	 */
	public static ErlProjectImport importProject(Backend b,
			final String prefix, final List<String> importSources) {
		OtpErlangObject res = null;
		try {
			res = b.call("erlide_import", "import", "sls", prefix,
					importSources);
			return new ErlProjectImport(res);
		} catch (final RpcException e) {
			ErlLogger.warn(e);
		}
		return null;
	}
}
