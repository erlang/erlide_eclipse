package org.erlide.backend.internal;

import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.IOtpRpcProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public class OtpRpcProvider implements IOtpRpcProvider {

    @Override
    public IOtpRpc get() {
        final IBackendManager backendManager = BackendCore.getBackendManager();
        final IBackend ideBackend = backendManager.getIdeBackend();
        return ideBackend.getOtpRpc();
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
