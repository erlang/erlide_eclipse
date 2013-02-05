package org.erlide.backend.internal;

import org.erlide.backend.BackendCore;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.IRuntimeProvider;

import com.ericsson.otp.erlang.RuntimeVersion;

public class BackendProvider implements IRuntimeProvider {

    @Override
    public IRpcSite get() {
        return BackendCore.getBackendManager().getIdeBackend().getRpcSite();
    }

    @Override
    public IRpcSite get(final RuntimeVersion version) {
        return BackendCore.getBackendManager().getByVersion(version);
    }

    @Override
    public IRpcSite get(final String name) {
        return BackendCore.getBackendManager().getByProject(name);
    }
}
