package org.erlide.backend.runtimeinfo;


public interface IRuntimeInfoSerializer {

    RuntimeInfoManagerData load();

    void store(RuntimeInfoManagerData data);

}
