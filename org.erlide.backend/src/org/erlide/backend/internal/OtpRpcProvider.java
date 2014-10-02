package org.erlide.backend.internal;

import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.IOtpRpcProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;

public class OtpRpcProvider implements IOtpRpcProvider {

    @Override
    public IOtpRpc get() {
        try {
            final IBackendManager backendManager = BackendCore.getBackendManager();
            final IBackend ideBackend = backendManager.getIdeBackend();
            return ideBackend.getOtpRpc();
        } catch (final Exception e) {
            ErlLogger.error("No Erlang runtime is installed!");
            return null;
        }
    }

    @Override
    public IOtpRpc get(final RuntimeVersion version) {
        return BackendCore.getBackendManager().getByVersion(version);
    }

    @Override
    public IOtpRpc get(final String projectName) {
        return BackendCore.getBackendManager().getByProject(projectName);
    }
}
