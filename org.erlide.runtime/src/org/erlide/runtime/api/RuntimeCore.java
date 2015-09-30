package org.erlide.runtime.api;

import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoSerializer;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalogData;
import org.erlide.util.HostnameUtils;

public class RuntimeCore {

    private static RuntimeInfoCatalog runtimeInfoCatalog;

    public static final synchronized IRuntimeInfoCatalog getRuntimeInfoCatalog(
            final IRuntimeInfoSerializer serializer) {
        if (runtimeInfoCatalog == null) {
            final RuntimeInfoCatalogData data = serializer.load();

            runtimeInfoCatalog = new RuntimeInfoCatalog();
            runtimeInfoCatalog.setRuntimes(data.runtimes, data.defaultRuntimeName,
                    data.erlideRuntimeName);
            for (final RuntimeInfo info : runtimeInfoCatalog.getRuntimes()) {
                // TODO detect only when necessary? this can block UI for a long time
                if (HostnameUtils.detectHostNames(info.getOtpHome())) {
                    break;
                }
            }
        }
        return runtimeInfoCatalog;
    }

    public static IRuntimeInfoCatalog getRuntimeInfoCatalog() {
        if (runtimeInfoCatalog != null) {
            return runtimeInfoCatalog;
        }
        return null;
    }

}
