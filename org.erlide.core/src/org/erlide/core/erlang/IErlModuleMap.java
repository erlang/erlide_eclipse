package org.erlide.core.erlang;

import java.util.List;
import java.util.Set;

public interface IErlModuleMap {

    void putEdited(String path);

    void removeEdited(String path);

    IErlModule getModuleByPath(String path);

    Set<IErlModule> getModulesByName(String moduleName);

    void putModule(IErlModule module);

    void removeModule(IErlModule module);

    void setIncludedFilesForModule(IErlModule module, List<IErlModule> result);

    List<IErlModule> getIncludedFilesForModule(IErlModule module);

}
