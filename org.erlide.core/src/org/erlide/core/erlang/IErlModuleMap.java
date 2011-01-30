package org.erlide.core.erlang;

import java.util.List;
import java.util.Set;

import erlang.ErlideOpen.ExternalTreeEntry;

public interface IErlModuleMap {

    void putEdited(String path);

    void removeEdited(String path);

    IErlModule getModuleByPath(String path);

    Set<IErlModule> getModulesByName(String moduleName);

    void putModule(IErlModule module);

    void removeModule(IErlModule module);

    void setIncludedFilesForModule(IErlModule module, List<IErlModule> result);

    List<IErlModule> getIncludedFilesForModule(IErlModule module);

    public List<ExternalTreeEntry> getExternalTree(final String externalPath);

    public void putExternalTree(final String externalPath,
            final List<ExternalTreeEntry> externalTree);

    List<IErlModule> getModulesForProject(IErlProject project);

    List<IErlModule> getIncludesForProject(IErlProject project);

    void setModulesForProject(IErlProject project,
            List<IErlModule> modulesAndIncludes);

    void setIncludesForProject(IErlProject project,
            List<IErlModule> modulesAndIncludes);

}
