package org.erlide.engine.util;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.IOtpRpcProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.services.ExtensionUtils;

public class OtpRpcFactory {

    private static IOtpRpcProvider provider;

    public static IOtpRpc getOtpRpc(final RuntimeVersion version) {
        if (provider == null) {
            provider = getOtpRpcProvider();
        }
        return provider.get(version);
    }

    public static IOtpRpc getOtpRpcForProject(final IErlProject project) {
        if (provider == null) {
            provider = getOtpRpcProvider();
        }
        return provider.get(project.getName());
    }

    public static IOtpRpc getOtpRpc() {
        if (provider == null) {
            provider = getOtpRpcProvider();
        }
        return provider.get();
    }

    private static IOtpRpcProvider getOtpRpcProvider() {
        return ExtensionUtils.getSingletonExtension("org.erlide.backend.backend",
                IOtpRpcProvider.class);
    }

}
