package org.erlide.core.erlang.internal;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.erlide.backend.util.IDisposable;
import org.erlide.backend.util.LRUCache;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModelChangeListener;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.ErlideUtil;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import erlang.ErlideOpen.ExternalTreeEntry;

public class ErlModelCache implements IDisposable {

    private static final int CACHE_SIZE = 100; // TODO make a more educated
                                               // guess here...
    private static ErlModelCache fgInstance = null;

    private final LRUCache<IErlModule, List<IErlModule>> moduleIncludeCache;
    private final LRUCache<String, IErlModule> pathToModuleCache;
    private final LRUCache<String, List<ExternalTreeEntry>> externalTreeCache;
    private final Map<String, IErlModule> editedModulesSet;
    private final Map<String, Set<IErlModule>> nameToModuleMap;
    private final ModelChangeListener modelChangeListener;
    private final LRUCache<IErlProject, List<IErlModule>> projectModuleCache;
    private final LRUCache<IErlProject, List<IErlModule>> projectIncludeCache;
    private final LRUCache<IErlProject, String> projectExternalModulesStringCache;
    private final LRUCache<IErlProject, String> projectExternalIncludesStringCache;
    private final LRUCache<IErlProject, Collection<IPath>> projectSourceDirsCache;
    private final LRUCache<IErlProject, Collection<IPath>> projectIncludeDirsCache;

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
        editedModulesSet = Maps.newHashMap();
        nameToModuleMap = Maps.newHashMap();
        moduleIncludeCache = new LRUCache<IErlModule, List<IErlModule>>(
                CACHE_SIZE);
        externalTreeCache = new LRUCache<String, List<ExternalTreeEntry>>(
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
        ErlangCore.getModel().addModelChangeListener(modelChangeListener);
    }

    public void putModule(final IErlModule module) {
        if (ErlideUtil.isNoModelCache()) {
            return;
        }
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
        if (ErlideUtil.isNoModelCache()) {
            return;
        }
        pathToModuleCache.put(path, module);
    }

    public void putEdited(final String path) {
        if (ErlideUtil.isNoModelCache()) {
            return;
        }
        final IErlModule module = pathToModuleCache.get(path);
        editedModulesSet.put(path, module);
    }

    public void removeEdited(final String path) {
        editedModulesSet.remove(path);
    }

    public IErlModule getModuleByPath(final String path) {
        final IErlModule module = editedModulesSet.get(path);
        if (module != null) {
            return module;
        }
        return pathToModuleCache.get(path);
    }

    public void putIncludedFilesForModule(final IErlModule module,
            final List<IErlModule> result) {
        if (ErlideUtil.isNoModelCache()) {
            return;
        }
        moduleIncludeCache.put(module, Lists.newArrayList(result));
    }

    public List<IErlModule> getIncludedFilesForModule(final IErlModule module) {
        final List<IErlModule> modules = moduleIncludeCache.get(module);
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
        if (ErlideUtil.isNoModelCache()) {
            return;
        }
        externalTreeCache.put(externalPath, Lists.newArrayList(externalTree));
    }

    public List<ExternalTreeEntry> getExternalTree(final String externalPath) {
        final List<ExternalTreeEntry> entries = externalTreeCache
                .get(externalPath);
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
        if (ErlideUtil.isNoModelCache()) {
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
        if (ErlideUtil.isNoModelCache()) {
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

    public void removeForProject(final IErlProject project) {
        // ErlLogger.debug("removeForProject %s", project.getName());
        projectIncludeCache.remove(project);
        projectModuleCache.remove(project);
    }

    public void pathVarsChanged() {
        // FIXME we need to clear some stuff here...
    }

    public String getExternalModulesString(final ErlProject project) {
        return projectExternalModulesStringCache.get(project);
    }

    public void putExternalModulesString(final ErlProject project,
            final String externalModulesString) {
        if (externalModulesString == null) {
            projectExternalModulesStringCache.remove(project);
        } else {
            projectExternalModulesStringCache.put(project,
                    externalModulesString);
        }
    }

    public String getExternalIncludesString(final ErlProject project) {
        return projectExternalIncludesStringCache.get(project);
    }

    public void putExternalIncludesString(final ErlProject project,
            final String externalModulesString) {
        if (externalModulesString == null) {
            projectExternalIncludesStringCache.remove(project);
        } else {
            projectExternalIncludesStringCache.put(project,
                    externalModulesString);
        }
    }

    public void putSourceDirs(final ErlProject project,
            final Collection<IPath> dirs) {
        if (dirs == null) {
            projectSourceDirsCache.remove(project);
        } else {
            projectSourceDirsCache.put(project, dirs);
        }
    }

    public Collection<IPath> getSourceDirs(final ErlProject project) {
        return projectSourceDirsCache.get(project);
    }

    public void putIncludeDirs(final ErlProject project,
            final Collection<IPath> dirs) {
        if (dirs == null) {
            projectIncludeDirsCache.remove(project);
        } else {
            projectIncludeDirsCache.put(project, dirs);
        }
    }

    public Collection<IPath> getIncludeDirs(final ErlProject project) {
        return projectIncludeDirsCache.get(project);
    }

}
