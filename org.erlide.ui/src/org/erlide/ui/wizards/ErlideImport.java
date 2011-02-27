/**
 *
 */
package org.erlide.ui.wizards;

import java.util.List;

import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.rpc.RpcCallSite;
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
    public static ErlProjectImport importProject(final RpcCallSite b,
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
