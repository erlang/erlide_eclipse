package org.erlide.runtime.runtimeinfo;

public interface IRuntimeInfoSerializer {

    RuntimeInfoCatalogData load();

    void store(RuntimeInfoCatalogData data);

}
