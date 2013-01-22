package org.erlide.runtime.runtimeinfo;

public interface IRuntimeInfoSerializer {

    RuntimeInfoManagerData load();

    void store(RuntimeInfoManagerData data);

}
