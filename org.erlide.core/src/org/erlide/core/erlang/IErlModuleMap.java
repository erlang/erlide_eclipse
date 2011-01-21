package org.erlide.core.erlang;

import java.util.Set;

public interface IErlModuleMap {

    void putEdited(String path);

    void removeEdited(String path);

    IErlModule getModuleByPath(String path);

    Set<IErlModule> getModulesByName(String moduleName);

    void putModule(IErlModule module);

    void removeModule(IErlModule module);

}
