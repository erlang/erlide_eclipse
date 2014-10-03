/**
 *
 */
package org.erlide.engine.internal.services.importer;

import java.util.List;

import org.erlide.engine.services.importer.ErlProjectImport;
import org.erlide.engine.services.importer.ImportService;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author jakob
 *
 */
public class ErlideImport implements ImportService {

    private final IOtpRpc backend;

    public ErlideImport(final IOtpRpc backend) {
        this.backend = backend;
    }

    /**
     * Filter import parameters for newly imported erlang project
     *
     * @param prefix
     * @param importSources
     * @param prefix
     *
     * @return
     */
    @Override
    public ErlProjectImport importProject(final String prefix,
            final List<String> importSources) {
        OtpErlangObject res = null;
        try {
            res = backend.call("erlide_import", "import", "sls", prefix, importSources);
            return new ErlProjectImport(res);
        } catch (final RpcException e) {
            ErlLogger.warn(e);
        }
        return null;
    }
}
