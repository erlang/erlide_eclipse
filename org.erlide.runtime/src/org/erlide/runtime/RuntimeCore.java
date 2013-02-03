package org.erlide.runtime;

import org.erlide.runtime.runtimeinfo.IRuntimeInfoSerializer;
import org.erlide.runtime.runtimeinfo.RuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfoManagerData;

public class RuntimeCore {

    private static RuntimeInfoCatalog runtimeInfoCatalog;

    public static final RuntimeInfoCatalog getRuntimeInfoCatalog(
            final IRuntimeInfoSerializer serializer) {
        if (runtimeInfoCatalog == null) {
            final RuntimeInfoManagerData data = serializer.load();

            runtimeInfoCatalog = new RuntimeInfoCatalog();
            runtimeInfoCatalog.setRuntimes(data.runtimes,
                    data.defaultRuntimeName, data.erlideRuntimeName);
            HostnameUtils.detectHostNames(runtimeInfoCatalog.erlideRuntime);
        }
        return runtimeInfoCatalog;
    }

    public static RuntimeInfoCatalog getRuntimeInfoCatalog() {
        if (runtimeInfoCatalog != null) {
            return runtimeInfoCatalog;
        }
        return null;
    }

}
