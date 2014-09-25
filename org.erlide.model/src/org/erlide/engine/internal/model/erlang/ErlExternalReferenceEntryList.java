package org.erlide.engine.internal.model.erlang;

import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.internal.model.root.Openable;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.OpenService;
import org.erlide.engine.services.search.OpenService.ExternalTreeEntry;
import org.erlide.engine.util.OtpRpcFactory;
import org.erlide.runtime.api.IOtpRpc;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Maps;

public class ErlExternalReferenceEntryList extends Openable implements IErlExternalRoot {

    private final String externalIncludes, externalModules;
    private final List<String> projectIncludes;

    public ErlExternalReferenceEntryList(final IParent parent, final String name,
            final String externalIncludes, final List<String> projectIncludes,
            final String externalModules) {
        super(parent, name);
        this.externalIncludes = externalIncludes;
        this.projectIncludes = projectIncludes;
        this.externalModules = externalModules;
    }

    @Override
    public ErlElementKind getKind() {
        return ErlElementKind.EXTERNAL_ROOT;
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm) throws ErlModelException {
        // TODO some code duplication within this function
        // ErlLogger.debug("ErlExternalReferenceEntryList.buildStructure %s",
        // getName());

        final IErlProject project = ErlangEngine.getInstance().getModelUtilService()
                .getProject(this);
        final ErlModelCache cache = ErlModelCache.getDefault();
        List<ExternalTreeEntry> externalModuleTree = cache
                .getExternalTree(externalModules);
        List<ExternalTreeEntry> externalIncludeTree = cache
                .getExternalTree(externalIncludes);
        if (externalModuleTree == null || externalIncludeTree == null) {
            final OtpErlangList pathVars = ErlangEngine.getInstance().getModel()
                    .getPathVars();
            final IOtpRpc backend = OtpRpcFactory.getOtpRpcForProject(project);
            if (externalModuleTree == null && externalModules.length() > 0) {
                if (pm != null) {
                    pm.worked(1);
                }
                externalModuleTree = ErlangEngine.getInstance()
                        .getService(OpenService.class)
                        .getExternalModuleTree(backend, externalModules, pathVars);
            }
            if (externalIncludeTree == null && externalIncludes.length() > 0) {
                if (pm != null) {
                    pm.worked(1);
                }
                externalIncludeTree = ErlangEngine.getInstance()
                        .getService(OpenService.class)
                        .getExternalModuleTree(backend, externalIncludes, pathVars);
            }
        }
        setChildren(null);
        final IErlModel model = ErlangEngine.getInstance().getModel();
        if (externalModuleTree != null && !externalModuleTree.isEmpty()) {
            addExternalEntries(pm, externalModuleTree, model, "modules", null, false);
            cache.putExternalTree(externalModules, project, externalModuleTree);
        }
        if (externalIncludeTree != null && !externalIncludeTree.isEmpty()
                || !projectIncludes.isEmpty()) {
            addExternalEntries(pm, externalIncludeTree, model, "includes",
                    projectIncludes, true);
            if (externalIncludeTree != null) {
                cache.putExternalTree(externalIncludes, project, externalIncludeTree);
            }
        }
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm,
            final List<ExternalTreeEntry> externalTree, final IErlModel model,
            final String rootName, final List<String> otherItems, final boolean includeDir)
            throws ErlModelException {
        final Map<String, IErlExternal> pathToEntryMap = Maps.newHashMap();
        pathToEntryMap.put("root", this);
        IErlExternal parent = null;
        if (externalTree != null && !externalTree.isEmpty()) {
            for (final ExternalTreeEntry entry : externalTree) {
                final String path = entry.getPath();
                // final String name = entry.getName();
                parent = pathToEntryMap.get(entry.getParentPath());
                if (entry.isModule()) {
                    final IErlModule module = model.getModuleFromFile(parent,
                            getNameFromPath(path), path, null, path);
                    parent.addChild(module);
                } else {
                    final String name = getNameFromExternalPath(path);
                    final ErlExternalReferenceEntry externalReferenceEntry = new ErlExternalReferenceEntry(
                            parent, name, path, true, includeDir);
                    pathToEntryMap.put(path, externalReferenceEntry);
                    externalReferenceEntry.open(pm);
                    parent.addChild(externalReferenceEntry);
                }
            }
        }
        if (otherItems != null) {
            if (parent == null) {
                parent = new ErlExternalReferenceEntry(this, rootName, "." + rootName
                        + ".", true, includeDir);
                addChild(parent);
            }
            for (final String path : otherItems) {
                final IErlModule module = model.getModuleFromFile(parent,
                        getNameFromPath(path), path, null, path);
                parent.addChild(module);
            }
        }
    }

    private String getNameFromPath(final String path) {
        final IPath p = new Path(path);
        final String name = p.lastSegment();
        return name;
    }

    private static String getNameFromExternalPath(final String path0) {
        String path = path0;
        int i = path.indexOf(".settings");
        if (i > 2) {
            path = path.substring(0, i - 1);
        }
        i = path.lastIndexOf('/');
        path = path.substring(i + 1);
        if (path.endsWith(".erlidex")) {
            path = path.substring(0, path.length() - 8);
        }
        return path;
    }

    @Override
    public String getFilePath() {
        return null;
    }

    public IOtpRpc getBackend() {
        return null;
    }

    @Override
    public boolean isOTP() {
        return false;
    }

    @Override
    public IResource getResource() {
        return null;
    }

    @Override
    public boolean hasIncludes() {
        return true;
    }

}
