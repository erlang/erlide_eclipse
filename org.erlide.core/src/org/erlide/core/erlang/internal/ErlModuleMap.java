package org.erlide.core.erlang.internal;

import java.util.Map;
import java.util.Set;

import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModuleMap;
import org.erlide.jinterface.backend.util.LRUCache;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class ErlModuleMap implements IErlModuleMap {

    private static final int CACHE_SIZE = 100; // TODO make a more educated
                                               // guess here...

    static ErlModuleMap erlModelMap = null;

    private final LRUCache<String, IErlModule> pathToModuleCache;
    private final Map<String, IErlModule> edited;
    private final Map<String, Set<IErlModule>> nameToModuleMap;

    public static ErlModuleMap getDefault() {
        if (erlModelMap == null) {
            erlModelMap = new ErlModuleMap();
        }
        return erlModelMap;
    }

    private ErlModuleMap() {
        pathToModuleCache = new LRUCache<String, IErlModule>(CACHE_SIZE);
        edited = Maps.newHashMap();
        nameToModuleMap = Maps.newHashMap();
    }

    public void putModule(final IErlModule module) {
        final String moduleName = module.getModuleName();
        Set<IErlModule> modules = getModulesByName(moduleName);
        if (modules == null) {
            modules = Sets.newHashSet();
        }
        modules.add(module);
        nameToModuleMap.put(moduleName, modules);
        final String path = module.getFilePath();
        pathToModuleCache.put(path, module);
    }

    public void removeModule(final IErlModule module) {
        final String moduleName = module.getModuleName();
        final Set<IErlModule> modules = getModulesByName(moduleName);
        if (modules != null) {
            modules.remove(moduleName);
            nameToModuleMap.put(moduleName, modules);
        }
    }

    public Set<IErlModule> getModulesByName(final String moduleName) {
        Set<IErlModule> modules = nameToModuleMap.get(moduleName);
        if (modules == null) {
            modules = Sets.newHashSet();
        }
        return modules;
    }

    public void put(final String path, final IErlModule module) {
        pathToModuleCache.put(path, module);
    }

    public void putEdited(final String path) {
        final IErlModule module = pathToModuleCache.get(path);
        edited.put(path, module);
    }

    public void removeEdited(final String path) {
        edited.remove(path);
    }

    public IErlModule getModuleByPath(final String path) {
        final IErlModule module = edited.get(path);
        if (module != null) {
            return module;
        }
        return pathToModuleCache.get(path);
    }

}
