/**
 *
 */
package org.erlide.ui.wizards;

import java.util.List;

import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;

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
    public static ErlProjectImport importProject(final IBackend b,
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
