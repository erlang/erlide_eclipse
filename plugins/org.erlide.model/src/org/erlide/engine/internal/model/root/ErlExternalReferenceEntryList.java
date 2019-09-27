package org.erlide.engine.internal.model.root;

import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.cache.ErlModelCache;
import org.erlide.engine.model.ErlElementKind;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.OtpRpcFactory;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlExternalRoot;
import org.erlide.engine.model.root.IErlModel;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.services.search.ExternalTreeEntry;
import org.erlide.runtime.rpc.IOtpRpc;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Maps;

public class ErlExternalReferenceEntryList extends Openable implements IErlExternalRoot {

    private final String externalIncludes;
    private final String externalModules;
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
            if (externalModuleTree == null && !externalModules.isEmpty()) {
                if (pm != null) {
                    pm.worked(1);
                }
                externalModuleTree = ErlangEngine.getInstance().getOpenService()
                        .getExternalModuleTree(backend, externalModules, pathVars);
            }
            if (externalIncludeTree == null && !externalIncludes.isEmpty()) {
                if (pm != null) {
                    pm.worked(1);
                }
                externalIncludeTree = ErlangEngine.getInstance().getOpenService()
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
            final String rootName, final List<String> otherItems,
            final boolean includeDir) throws ErlModelException {
        final Map<IPath, IErlExternal> pathToEntryMap = Maps.newHashMap();
        pathToEntryMap.put(new Path("root"), this);
        IErlExternal parent = null;
        if (externalTree != null && !externalTree.isEmpty()) {
            for (final ExternalTreeEntry entry : externalTree) {
                final IPath path = entry.getPath();
                parent = pathToEntryMap.get(entry.getParentPath());
                if (entry.isModule()) {
                    final IErlModule module = model.getModuleFromFile(parent,
                            getNameFromPath(path), path, null);
                    parent.addChild(module);
                } else {
                    final String name = ErlExternalReferenceEntryList
                            .getNameFromExternalPath(path);
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
                parent = new ErlExternalReferenceEntry(this, rootName,
                        new Path("." + rootName + "."), true, includeDir);
                addChild(parent);
            }
            for (final String path : otherItems) {
                final IPath apath = new Path(path);
                final IErlModule module = model.getModuleFromFile(parent,
                        getNameFromPath(apath), apath, null);
                parent.addChild(module);
            }
        }
    }

    private String getNameFromPath(final IPath path) {
        final String name = path.lastSegment();
        return name;
    }

    private static String getNameFromExternalPath(final IPath path) {
        String name = path.lastSegment();
        if (name.endsWith(".erlidex")) {
            name = name.substring(0, name.length() - ".erlidex".length());
        }
        return name;
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
