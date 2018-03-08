package org.erlide.engine.model;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.rpc.IOtpRpcProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.services.ExtensionUtils;

public class OtpRpcFactory {

    private static IOtpRpcProvider provider;

    public static IOtpRpc getOtpRpc(final RuntimeVersion version) {
        if (OtpRpcFactory.provider == null) {
            OtpRpcFactory.provider = OtpRpcFactory.getOtpRpcProvider();
        }
        return OtpRpcFactory.provider.get(version);
    }

    public static IOtpRpc getOtpRpcForProject(final IErlProject project) {
        if (OtpRpcFactory.provider == null) {
            OtpRpcFactory.provider = OtpRpcFactory.getOtpRpcProvider();
        }
        return OtpRpcFactory.provider.get(project.getName());
    }

    public static IOtpRpc getOtpRpc() {
        if (OtpRpcFactory.provider == null) {
            OtpRpcFactory.provider = OtpRpcFactory.getOtpRpcProvider();
        }
        return OtpRpcFactory.provider.get();
    }

    private static IOtpRpcProvider getOtpRpcProvider() {
        return ExtensionUtils.getSingletonExtension("org.erlide.backend.backend",
                IOtpRpcProvider.class);
    }

}
