package org.erlide.core.internal.model.root;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.CoreScope;
import org.erlide.core.common.IDisposable;
import org.erlide.core.common.Tuple;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlModelChangeListener;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.model.util.ErlideUtil;
import org.erlide.core.services.search.ErlideOpen.ExternalTreeEntry;
import org.erlide.jinterface.util.LRUCache;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class ErlModelCache implements IDisposable {

    private static final int CACHE_SIZE = 100;
    // TODO make a more educated guess here...
    // private static final int NAME_CACHE_SIZE = 300;
    private static ErlModelCache fgInstance = null;

    private final LRUCache<IErlModule, List<IErlModule>> moduleIncludeCache;
    private final LRUCache<String, IErlModule> pathToModuleCache;
    private final LRUCache<String, Tuple<IErlProject, List<ExternalTreeEntry>>> externalTreeCache;
    private final Map<String, IErlModule> editedModulesMap;
    // private final LRUCache<String, Set<IErlModule>> nameToModuleCache;
    private final ModelChangeListener modelChangeListener;
    private final LRUCache<IErlProject, List<IErlModule>> projectModuleCache;
    private final LRUCache<IErlProject, List<IErlModule>> projectIncludeCache;
    private final LRUCache<IErlProject, String> projectExternalModulesStringCache;
    private final LRUCache<IErlProject, String> projectExternalIncludesStringCache;
    private final LRUCache<IErlProject, Collection<IPath>> projectSourceDirsCache;
    private final LRUCache<IErlProject, Collection<IPath>> projectIncludeDirsCache;
    private final boolean disabled;

    public static ErlModelCache getDefault() {
        if (fgInstance == null) {
            fgInstance = new ErlModelCache();
        }
        return fgInstance;
    }

    private class ModelChangeListener implements IErlModelChangeListener {
        // TODO should we handle changes of projects and includes too?
        // Which is hard, since the IOldErlangProjectProperties doesn't have
        // listeners
        public void elementChanged(final IErlElement element) {
            if (element instanceof IErlModule) {
                final IErlModule module = (IErlModule) element;
                moduleIncludeCache.remove(module);
            }
        }
    }

    private ErlModelCache() {
        pathToModuleCache = new LRUCache<String, IErlModule>(CACHE_SIZE);
        editedModulesMap = Maps.newHashMap();
        // nameToModuleCache = new LRUCache<String, Set<IErlModule>>(
        // NAME_CACHE_SIZE);
        moduleIncludeCache = new LRUCache<IErlModule, List<IErlModule>>(
                CACHE_SIZE);
        externalTreeCache = new LRUCache<String, Tuple<IErlProject, List<ExternalTreeEntry>>>(
                CACHE_SIZE);
        projectModuleCache = new LRUCache<IErlProject, List<IErlModule>>(
                CACHE_SIZE);
        projectIncludeCache = new LRUCache<IErlProject, List<IErlModule>>(
                CACHE_SIZE);
        projectExternalModulesStringCache = new LRUCache<IErlProject, String>(
                CACHE_SIZE);
        projectExternalIncludesStringCache = new LRUCache<IErlProject, String>(
                CACHE_SIZE);
        projectSourceDirsCache = new LRUCache<IErlProject, Collection<IPath>>(
                CACHE_SIZE);
        projectIncludeDirsCache = new LRUCache<IErlProject, Collection<IPath>>(
                CACHE_SIZE);
        modelChangeListener = new ModelChangeListener();
        CoreScope.getModel().addModelChangeListener(modelChangeListener);
        disabled = ErlideUtil.isCacheDisabled();
    }

    public void putModule(final IErlModule module) {
        if (disabled) {
            return;
        }
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
            pathToModuleCache.remove(path);
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
        if (disabled) {
            return;
        }
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
        return pathToModuleCache.get(path);
    }

    public void putIncludedFilesForModule(final IErlModule module,
            final List<IErlModule> result) {
        if (disabled) {
            return;
        }
        if (result == null) {
            moduleIncludeCache.remove(module);
        } else {
            moduleIncludeCache.put(module, Lists.newArrayList(result));
        }
    }

    public List<IErlModule> getIncludedFilesForModule(final IErlModule module) {
        final List<IErlModule> modules = moduleIncludeCache.get(module);
        if (modules == null) {
            return Lists.newArrayList();
        }
        return Lists.newArrayList(modules);
    }

    public void dispose() {
        CoreScope.getModel().removeModelChangeListener(modelChangeListener);
    }

    public void putExternalTree(final String externalPath,
            final IErlProject project,
            final List<ExternalTreeEntry> externalTree) {
        if (disabled) {
            return;
        }
        if (externalTree == null) {
            externalTreeCache.remove(externalPath);
        } else {
            externalTreeCache.put(externalPath,
                    new Tuple<IErlProject, List<ExternalTreeEntry>>(project,
                            Lists.newArrayList(externalTree)));
        }
    }

    public List<ExternalTreeEntry> getExternalTree(final String externalPath) {
        final Tuple<IErlProject, List<ExternalTreeEntry>> tuple = externalTreeCache
                .get(externalPath);
        if (tuple == null) {
            return null;
        }
        final List<ExternalTreeEntry> entries = tuple.o2;
        if (entries == null) {
            return null;
        }
        return Lists.newArrayList(entries);
    }

    public List<IErlModule> getModulesForProject(final IErlProject project) {
        final List<IErlModule> modules = projectModuleCache.get(project);
        if (modules == null) {
            return null;
        }
        return Lists.newArrayList(modules);
    }

    public List<IErlModule> getIncludesForProject(final IErlProject project) {
        final List<IErlModule> includes = projectIncludeCache.get(project);
        if (includes == null) {
            return null;
        }
        return Lists.newArrayList(includes);
    }

    public void putModulesForProject(final IErlProject project,
            final List<IErlModule> modules) {
        if (disabled) {
            return;
        }
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
        if (disabled) {
            return;
        }
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
        final List<IErlModule> includes = projectIncludeCache.get(project);
        if (includes != null) {
            for (final IErlModule module : includes) {
                moduleIncludeCache.put(module, null);
            }
        }
        final List<IErlModule> modules = projectModuleCache.get(project);
        if (modules != null) {
            for (final IErlModule module : modules) {
                moduleIncludeCache.put(module, null);
            }
        }
        projectIncludeCache.remove(project);
        projectModuleCache.remove(project);
        final Set<String> keysToRemove = Sets.newHashSet();
        final Set<Entry<String, Tuple<IErlProject, List<ExternalTreeEntry>>>> entrySet = externalTreeCache
                .entrySet();
        for (final Entry<String, Tuple<IErlProject, List<ExternalTreeEntry>>> entry : entrySet) {
            if (entry.getValue().o1 == project) {
                keysToRemove.add(entry.getKey());
            }
        }
        for (final String keyToRemove : keysToRemove) {
            externalTreeCache.remove(keyToRemove);
        }
        projectExternalModulesStringCache.remove(project);
        projectExternalIncludesStringCache.remove(project);
        projectIncludeDirsCache.remove(project);
        projectSourceDirsCache.remove(project);
    }

    public void pathVarsChanged() {
        // FIXME we need to clear some stuff here...
    }

    public String getExternalModulesString(final IErlProject project) {
        return projectExternalModulesStringCache.get(project);
    }

    public void putExternalModulesString(final IErlProject project,
            final String externalModulesString) {
        if (disabled) {
            return;
        }
        if (externalModulesString == null) {
            projectExternalModulesStringCache.remove(project);
        } else {
            projectExternalModulesStringCache.put(project,
                    externalModulesString);
        }
    }

    public String getExternalIncludesString(final IErlProject project) {
        return projectExternalIncludesStringCache.get(project);
    }

    public void putExternalIncludesString(final IErlProject project,
            final String externalModulesString) {
        if (disabled) {
            return;
        }
        if (externalModulesString == null) {
            projectExternalIncludesStringCache.remove(project);
        } else {
            projectExternalIncludesStringCache.put(project,
                    externalModulesString);
        }
    }

    public void putSourceDirs(final IErlProject project,
            final Collection<IPath> dirs) {
        if (disabled) {
            return;
        }
        if (dirs == null) {
            projectSourceDirsCache.remove(project);
        } else {
            projectSourceDirsCache.put(project, dirs);
        }
    }

    public Collection<IPath> getSourceDirs(final IErlProject project) {
        return projectSourceDirsCache.get(project);
    }

    public void putIncludeDirs(final IErlProject project,
            final Collection<IPath> dirs) {
        if (disabled) {
            return;
        }
        if (dirs == null) {
            projectIncludeDirsCache.remove(project);
        } else {
            projectIncludeDirsCache.put(project, dirs);
        }
    }

    public Collection<IPath> getIncludeDirs(final IErlProject project) {
        return projectIncludeDirsCache.get(project);
    }

    public void newProjectCreated() {
        pathToModuleCache.clear();
        // nameToModuleCache.clear();
    }

    public void putModules(final Collection<IErlModule> modules) {
        for (final IErlModule module : modules) {
            putModule(module);
        }
    }

    public void clearModelCache() {
        moduleIncludeCache.clear();
        pathToModuleCache.clear();
        externalTreeCache.clear();
        // editedModulesMap.clear();
        // nameToModuleCache.clear();
        projectModuleCache.clear();
        projectIncludeCache.clear();
        projectExternalModulesStringCache.clear();
        projectExternalIncludesStringCache.clear();
        projectSourceDirsCache.clear();
        projectIncludeDirsCache.clear();
    }
}
