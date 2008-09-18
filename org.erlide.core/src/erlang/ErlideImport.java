/**
 * 
 */
package erlang;

import java.util.List;

import org.erlide.core.erlang.ErlProjectImport;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.IdeBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

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
	public static ErlProjectImport importProject(IdeBackend b,
			final String prefix, final List<String> importSources) {
		OtpErlangObject res = null;
		try {
			res = b.rpcx("erlide_import", "import", "sls", prefix,
					importSources);
			return new ErlProjectImport(res);
		} catch (final RpcException e) {
			e.printStackTrace();
		} catch (final BackendException e) {
			e.printStackTrace();
		}
		return null;
	}
}
