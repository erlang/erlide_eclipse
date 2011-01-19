package org.erlide.core.erlang.internal;

import java.util.Map;
import java.util.Set;

import org.erlide.core.erlang.IErlModelMap;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModuleInternal;
import org.erlide.jinterface.backend.util.LRUCache;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class ErlModelMap implements IErlModelMap {

    private static final int CACHE_SIZE = 100; // TODO make a more educated
                                               // guess here...

    static ErlModelMap erlModelMap = null;

    private final LRUCache<String, IErlModuleInternal> pathToModuleInternalCache;
    private final Map<String, IErlModuleInternal> edited;
    private final Map<String, Set<IErlModule>> nameToModuleMap;

    public static ErlModelMap getDefault() {
        if (erlModelMap == null) {
            erlModelMap = new ErlModelMap();
        }
        return erlModelMap;
    }

    private ErlModelMap() {
        pathToModuleInternalCache = new LRUCache<String, IErlModuleInternal>(
                CACHE_SIZE);
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
        return nameToModuleMap.get(moduleName);
    }

    public void put(final String path, final IErlModuleInternal moduleInternal) {
        pathToModuleInternalCache.put(path, moduleInternal);
    }

    public void putEdited(final String path) {
        final IErlModuleInternal moduleInternal = pathToModuleInternalCache
                .get(path);
        edited.put(path, moduleInternal);
    }

    public void removeEdited(final String path) {
        edited.remove(path);
    }

    public IErlModuleInternal get(final String path) {
        final IErlModuleInternal moduleInternal = edited.get(path);
        if (moduleInternal != null) {
            return moduleInternal;
        }
        return pathToModuleInternalCache.get(path);
    }

    public boolean has(final String path) {
        return pathToModuleInternalCache.containsKey(path);
    }

    /**
     * Is this really needed?
     * 
     * @param moduleInternal
     * @return
     */
    public boolean has(final IErlModuleInternal moduleInternal) {
        return pathToModuleInternalCache.containsValue(moduleInternal);
    }

}