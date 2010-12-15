/**
 *
 */
package erlang;

import java.util.List;

import org.erlide.core.erlang.ErlProjectImport;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;

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
     * @param importSources
     * @param prefix
     * 
     * @return
     */
    public static ErlProjectImport importProject(final Backend b,
            final String prefix, final List<String> importSources) {
        OtpErlangObject res = null;
        try {
            res = b.call("erlide_import", "import", "sls", prefix,
                    importSources);
            return new ErlProjectImport(res);
        } catch (final BackendException e) {
            ErlLogger.warn(e);
        }
        return null;
    }
}
