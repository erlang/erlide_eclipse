package org.erlide.core.erlang.internal;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModuleMap;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.util.LRUCache;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import erlang.ErlideOpen.ExternalTreeEntry;

public class ErlModuleMap implements IErlModuleMap, IDisposable {

    private static final int CACHE_SIZE = 100; // TODO make a more educated
                                               // guess here...
    private static IErlModuleMap erlModelMap = null;

    private final LRUCache<IErlModule, List<IErlModule>> moduleIncludeMap;
    private final LRUCache<String, IErlModule> pathToModuleCache;
    private final LRUCache<String, List<ExternalTreeEntry>> externalTreeMap;
    private final Map<String, IErlModule> edited;
    private final Map<String, Set<IErlModule>> nameToModuleMap;
    private final ModelChangeListener modelChangeListener;

    public static IErlModuleMap getDefault() {
        if (erlModelMap == null) {
            erlModelMap = new ErlModuleMap();
        }
        return erlModelMap;
    }

    private class ModelChangeListener implements IErlModelChangeListener {

        public void elementChanged(final IErlElement element) {
            if (element instanceof IErlModule) {
                final IErlModule module = (IErlModule) element;
                moduleIncludeMap.remove(module);
            }
        }

    }

    private ErlModuleMap() {
        pathToModuleCache = new LRUCache<String, IErlModule>(CACHE_SIZE);
        edited = Maps.newHashMap();
        nameToModuleMap = Maps.newHashMap();
        moduleIncludeMap = new LRUCache<IErlModule, List<IErlModule>>(
                CACHE_SIZE);
        externalTreeMap = new LRUCache<String, List<ExternalTreeEntry>>(
                CACHE_SIZE);
        modelChangeListener = new ModelChangeListener();
        ErlangCore.getModel().addModelChangeListener(modelChangeListener);
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
        final Set<IErlModule> modules = nameToModuleMap.get(moduleName);
        if (modules == null) {
            return Sets.newHashSet();
        }
        return Sets.newHashSet(modules);
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

    public void setIncludedFilesForModule(final IErlModule module,
            final List<IErlModule> result) {
        moduleIncludeMap.put(module, Lists.newArrayList(result));
    }

    public List<IErlModule> getIncludedFilesForModule(final IErlModule module) {
        final List<IErlModule> modules = moduleIncludeMap.get(module);
        if (modules == null) {
            return Lists.newArrayList();
        }
        return Lists.newArrayList(modules);
    }

    public void dispose() {
        ErlangCore.getModel().removeModelChangeListener(modelChangeListener);
    }

    public void putExternalTree(final String externalPath,
            final List<ExternalTreeEntry> externalTree) {
        externalTreeMap.put(externalPath, Lists.newArrayList(externalTree));
    }

    public List<ExternalTreeEntry> getExternalTree(final String externalPath) {
        final List<ExternalTreeEntry> entries = externalTreeMap
                .get(externalPath);
        if (entries == null) {
            return null;
        }
        return Lists.newArrayList(entries);
    }
}
