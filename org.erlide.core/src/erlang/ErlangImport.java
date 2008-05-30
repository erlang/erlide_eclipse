/**
 * 
 */
package erlang;

import java.util.List;

import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author jakob
 * 
 */
public class ErlangImport {

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
	public static ErlProjectImport importProject(final String prefix,
			final List<String> importSources) {
		final IBackend b = BackendManager.getDefault().getIdeBackend();
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
