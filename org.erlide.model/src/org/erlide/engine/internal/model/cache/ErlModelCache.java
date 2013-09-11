package org.erlide.engine.internal.model.cache;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.util.ErlideUtil;
import org.erlide.engine.model.IErlModelChangeListener;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenService.ExternalTreeEntry;
import org.erlide.util.IDisposable;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class ErlModelCache implements IDisposable {

    private static final int CACHE_SIZE = 100;
    private static final int CACHE_TIME_MINUTES = 15;
    // TODO make a more educated guess here...
    // private static final int NAME_CACHE_SIZE = 300;

    private static final boolean disabled = ErlideUtil.isCacheDisabled();
    private static final ErlModelCache fgInstance = disabled ? new DisabledErlModelCache()
            : new ErlModelCache();

    private final Cache<IErlModule, List<IErlModule>> moduleIncludeCache;
    private final Cache<String, IErlModule> pathToModuleCache;
    private final Cache<String, Pair<IErlProject, List<ExternalTreeEntry>>> externalTreeCache;
    private final Map<String, IErlModule> editedModulesMap;
    // private final Cache<String, Set<IErlModule>> nameToModuleCache;
    private final ModelChangeListener modelChangeListener;
    private final Cache<IErlProject, List<IErlModule>> projectModuleCache;
    private final Cache<IErlProject, List<IErlModule>> projectIncludeCache;
    private final Cache<IErlProject, String> projectExternalModulesStringCache;
    private final Cache<IErlProject, String> projectExternalIncludesStringCache;
    private final Cache<IErlProject, Collection<IPath>> projectSourceDirsCache;
    private final Cache<IErlProject, Collection<IPath>> projectIncludeDirsCache;

    public static ErlModelCache getDefault() {
        return fgInstance;
    }

    private class ModelChangeListener implements IErlModelChangeListener {
        // TODO should we handle changes of projects and includes too?
        // Which is hard, since the IOldErlangProjectProperties doesn't have
        // listeners
        @Override
        public void elementChanged(final IErlElement element) {
            if (element instanceof IErlModule) {
                final IErlModule module = (IErlModule) element;
                moduleIncludeCache.invalidate(module);
            }
        }
    }

    private static <K, V> Cache<K, V> newCache() {
        final Cache<K, V> cache = CacheBuilder.newBuilder()
                .maximumSize(CACHE_SIZE)
                .expireAfterAccess(CACHE_TIME_MINUTES, TimeUnit.MINUTES)
                .initialCapacity(16).build();
        return cache;
    }

    private ErlModelCache() {
        pathToModuleCache = newCache();
        editedModulesMap = Maps.newHashMap();
        // nameToModuleCache = newCache();
        moduleIncludeCache = newCache();
        externalTreeCache = newCache();
        projectModuleCache = newCache();
        projectIncludeCache = newCache();
        projectExternalModulesStringCache = newCache();
        projectExternalIncludesStringCache = newCache();
        projectSourceDirsCache = newCache();
        projectIncludeDirsCache = newCache();

        modelChangeListener = new ModelChangeListener();
        ErlangEngine.getInstance().getModel()
                .addModelChangeListener(modelChangeListener);
    }

    public void putModule(final IErlModule module) {
        // final String moduleName = module.getModuleName();
        // Set<IErlModule> modules = getModulesByName(moduleName);
        // if (modules == null) {
        // modules = Sets.newHashSet();
        // }
        // modules.add(module);
        // nameToModuleCache.put(moduleName, modules);
        final String path = module.getFilePath();
        pathToModuleCache.put(path, module);
    }

    public void removeModule(final IErlModule module) {
        // final String moduleName = module.getModuleName();
        // nameToModuleCache.remove(moduleName);
        final String path = module.getFilePath();
        if (path != null) {
            pathToModuleCache.invalidate(path);
        }
    }

    // public Set<IErlModule> getModulesByName(final String moduleName) {
    // final Set<IErlModule> modules = nameToModuleCache.get(moduleName);
    // if (modules == null) {
    // return Sets.newHashSet();
    // }
    // return Sets.newHashSet(modules);
    // }

    // public void put(final String path, final IErlModule module) {
    // if (noModelCache) {
    // return;
    // }
    // pathToModuleCache.put(path, module);
    // }

    public void putEdited(final String path, final IErlModule module) {
        if (module == null) {
            editedModulesMap.remove(path);
        } else {
            editedModulesMap.put(path, module);
        }
    }

    public IErlModule getModuleByPath(final String path) {
        final IErlModule module = editedModulesMap.get(path);
        if (module != null) {
            return module;
        }
        return pathToModuleCache.getIfPresent(path);
    }

    public void putIncludedFilesForModule(final IErlModule module,
            final Collection<IErlModule> result) {
        if (result == null) {
            moduleIncludeCache.invalidate(module);
        } else {
            moduleIncludeCache.put(module, Lists.newArrayList(result));
        }
    }

    public List<IErlModule> getIncludedFilesForModule(final IErlModule module) {
        final List<IErlModule> modules = moduleIncludeCache
                .getIfPresent(module);
        if (modules == null) {
            return Lists.newArrayList();
        }
        return Lists.newArrayList(modules);
    }

    @Override
    public void dispose() {
        ErlangEngine.getInstance().getModel()
                .removeModelChangeListener(modelChangeListener);
    }

    public void putExternalTree(final String externalPath,
            final IErlProject project,
            final List<ExternalTreeEntry> externalTree) {
        if (externalTree == null) {
            externalTreeCache.invalidate(externalPath);
        } else {
            externalTreeCache.put(externalPath,
                    new Pair<IErlProject, List<ExternalTreeEntry>>(project,
                            Lists.newArrayList(externalTree)));
        }
    }

    public List<ExternalTreeEntry> getExternalTree(final String externalPath) {
        final Pair<IErlProject, List<ExternalTreeEntry>> tuple = externalTreeCache
                .getIfPresent(externalPath);
        if (tuple == null) {
            return null;
        }
        final List<ExternalTreeEntry> entries = tuple.getValue();
        if (entries == null) {
            return null;
        }
        return Lists.newArrayList(entries);
    }

    public List<IErlModule> getModulesForProject(final IErlProject project) {
        final List<IErlModule> modules = projectModuleCache
                .getIfPresent(project);
        if (modules == null) {
            return null;
        }
        return Lists.newArrayList(modules);
    }

    public List<IErlModule> getIncludesForProject(final IErlProject project) {
        final List<IErlModule> includes = projectIncludeCache
                .getIfPresent(project);
        if (includes == null) {
            return null;
        }
        return Lists.newArrayList(includes);
    }

    public void putModulesForProject(final IErlProject project,
            final List<IErlModule> modules) {
        final List<String> moduleNames = Lists.newArrayList();
        for (final IErlModule module : modules) {
            moduleNames.add(module.getName());
        }
        // ErlLogger.debug("setModulesForProject %s %s", project.getName(),
        // moduleNames);
        projectModuleCache.put(project, Lists.newArrayList(modules));
    }

    public void putIncludesForProject(final IErlProject project,
            final List<IErlModule> includes) {
        final List<String> moduleNames = Lists.newArrayList();
        for (final IErlModule module : includes) {
            moduleNames.add(module.getName());
        }
        // ErlLogger.debug("setIncludesForProject %s %s", project.getName(),
        // moduleNames);
        projectIncludeCache.put(project, Lists.newArrayList(includes));
    }

    public void removeProject(final IErlProject project) {
        // ErlLogger.debug("removeForProject %s", project.getName());
        final List<IErlModule> includes = projectIncludeCache
                .getIfPresent(project);
        if (includes != null) {
            for (final IErlModule module : includes) {
                moduleIncludeCache.invalidate(module);
            }
        }
        final List<IErlModule> modules = projectModuleCache
                .getIfPresent(project);
        if (modules != null) {
            for (final IErlModule module : modules) {
                moduleIncludeCache.invalidate(module);
            }
        }
        projectIncludeCache.invalidate(project);
        projectModuleCache.invalidate(project);
        final Set<String> keysToRemove = Sets.newHashSet();
        final Set<Entry<String, Pair<IErlProject, List<ExternalTreeEntry>>>> entrySet = externalTreeCache
                .asMap().entrySet();
        for (final Entry<String, Pair<IErlProject, List<ExternalTreeEntry>>> entry : entrySet) {
            if (entry.getValue().getKey() == project) {
                keysToRemove.add(entry.getKey());
            }
        }
        for (final String keyToRemove : keysToRemove) {
            externalTreeCache.invalidate(keyToRemove);
        }
        projectExternalModulesStringCache.invalidate(project);
        projectExternalIncludesStringCache.invalidate(project);
        projectIncludeDirsCache.invalidate(project);
        projectSourceDirsCache.invalidate(project);
    }

    public void pathVarsChanged() {
        // FIXME we need to clear some stuff here...
    }

    public String getExternalModulesString(final IErlProject project) {
        return projectExternalModulesStringCache.getIfPresent(project);
    }

    public void putExternalModulesString(final IErlProject project,
            final String externalModulesString) {
        if (externalModulesString == null) {
            projectExternalModulesStringCache.invalidate(project);
        } else {
            projectExternalModulesStringCache.put(project,
                    externalModulesString);
        }
    }

    public String getExternalIncludesString(final IErlProject project) {
        return projectExternalIncludesStringCache.getIfPresent(project);
    }

    public void putExternalIncludesString(final IErlProject project,
            final String externalModulesString) {
        if (externalModulesString == null) {
            projectExternalIncludesStringCache.invalidate(project);
        } else {
            projectExternalIncludesStringCache.put(project,
                    externalModulesString);
        }
    }

    public void putSourceDirs(final IErlProject project,
            final Collection<IPath> dirs) {
        if (dirs == null) {
            projectSourceDirsCache.invalidate(project);
        } else {
            projectSourceDirsCache.put(project, dirs);
        }
    }

    public Collection<IPath> getSourceDirs(final IErlProject project) {
        return projectSourceDirsCache.getIfPresent(project);
    }

    public void putIncludeDirs(final IErlProject project,
            final Collection<IPath> dirs) {
        if (dirs == null) {
            projectIncludeDirsCache.invalidate(project);
        } else {
            projectIncludeDirsCache.put(project, dirs);
        }
    }

    public Collection<IPath> getIncludeDirs(final IErlProject project) {
        return projectIncludeDirsCache.getIfPresent(project);
    }

    public void newProjectCreated() {
        pathToModuleCache.invalidateAll();
        // nameToModuleCache.clear();
    }

    public void putModules(final Collection<IErlModule> modules) {
        for (final IErlModule module : modules) {
            putModule(module);
        }
    }

    public void clearModelCache() {
        moduleIncludeCache.invalidateAll();
        pathToModuleCache.invalidateAll();
        externalTreeCache.invalidateAll();
        // editedModulesMap.clear();
        // nameToModuleCache.clear();
        projectModuleCache.invalidateAll();
        projectIncludeCache.invalidateAll();
        projectExternalModulesStringCache.invalidateAll();
        projectExternalIncludesStringCache.invalidateAll();
        projectSourceDirsCache.invalidateAll();
        projectIncludeDirsCache.invalidateAll();
    }

    private static class DisabledErlModelCache extends ErlModelCache {
        @Override
        public void putEdited(final String path, final IErlModule module) {
        }

        @Override
        public void putExternalIncludesString(final IErlProject project,
                final String externalModulesString) {
        }

        @Override
        public void putExternalModulesString(final IErlProject project,
                final String externalModulesString) {
        }

        @Override
        public void putExternalTree(final String externalPath,
                final IErlProject project,
                final List<ExternalTreeEntry> externalTree) {
        }

        @Override
        public void putIncludedFilesForModule(final IErlModule module,
                final Collection<IErlModule> result) {
        }

        @Override
        public void putIncludeDirs(final IErlProject project,
                final Collection<IPath> dirs) {
        }

        @Override
        public void putIncludesForProject(final IErlProject project,
                final List<IErlModule> includes) {
        }

        @Override
        public void putModule(final IErlModule module) {
        }

        @Override
        public void putModules(final Collection<IErlModule> modules) {
        }

        @Override
        public void putModulesForProject(final IErlProject project,
                final List<IErlModule> modules) {
        }

        @Override
        public void putSourceDirs(final IErlProject project,
                final Collection<IPath> dirs) {
        }
    }
}
