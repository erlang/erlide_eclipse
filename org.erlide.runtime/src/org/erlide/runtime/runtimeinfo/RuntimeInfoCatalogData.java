package org.erlide.runtime.runtimeinfo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public final class RuntimeInfoCatalogData {
    public final String erlideRuntimeName;
    public final Collection<RuntimeInfo> runtimes;
    public final String defaultRuntimeName;

    public RuntimeInfoCatalogData(final Collection<RuntimeInfo> runtimes,
            final String defaultRuntimeName, final String erlideRuntimeName) {
        this.runtimes = Collections.unmodifiableCollection(runtimes);
        this.defaultRuntimeName = defaultRuntimeName;
        this.erlideRuntimeName = erlideRuntimeName;
    }

    public RuntimeInfoCatalogData() {
        this(new ArrayList<RuntimeInfo>(), null, null);
    }
}
