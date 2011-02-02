package org.erlide.core.erlang.internal;

import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlModuleMap;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Maps;

import erlang.ErlideOpen;
import erlang.ErlideOpen.ExternalTreeEntry;

public class ErlExternalReferenceEntryList extends Openable implements
        IErlExternal {

    private final String externalIncludes, externalModules;
    private final String externalName;

    public ErlExternalReferenceEntryList(final IParent parent,
            final String name, final String externalName,
            final String externalIncludes, final String externalModules) {
        super(parent, name);
        this.externalName = externalName;
        this.externalIncludes = externalIncludes;
        this.externalModules = externalModules;
    }

    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        // TODO some code duplication within this function
        ErlLogger.debug("ErlExternalReferenceEntryList.buildStructure %s",
                externalName);
        final IErlModuleMap moduleMap = ErlangCore.getModuleMap();
        List<ExternalTreeEntry> externalModuleTree = moduleMap
                .getExternalTree(externalModules);
        List<ExternalTreeEntry> externalIncludeTree = moduleMap
                .getExternalTree(externalIncludes);
        if (externalModuleTree == null || externalIncludeTree == null) {
            final Backend backend = BackendUtils
                    .getBuildOrIdeBackend(getErlProject().getProject());
            final OtpErlangList pathVars = ErlangCore.getModel().getPathVars();
            if (externalModuleTree == null && externalModules.length() > 0) {
                if (pm != null) {
                    pm.worked(1);
                }
                externalModuleTree = ErlideOpen.getExternalModuleTree(backend,
                        externalModules, pathVars);
            }
            if (externalIncludeTree == null && externalIncludes.length() > 0) {
                if (pm != null) {
                    pm.worked(1);
                }
                externalIncludeTree = ErlideOpen.getExternalModuleTree(backend,
                        externalIncludes, pathVars);
            }
        }
        final IErlModelManager modelManager = ErlangCore.getModelManager();
        removeChildren();
        if (externalModuleTree != null && !externalModuleTree.isEmpty()) {
            addExternalEntries(pm, externalModuleTree, modelManager, "modules",
                    externalModules);
            moduleMap.putExternalTree(externalModules, externalModuleTree);
        }
        if (externalIncludeTree != null && !externalIncludeTree.isEmpty()) {
            addExternalEntries(pm, externalIncludeTree, modelManager,
                    "includes", externalIncludes);
            moduleMap.putExternalTree(externalIncludes, externalIncludeTree);
        }
        return true;
    }

    private void addExternalEntries(final IProgressMonitor pm,
            final List<ExternalTreeEntry> externalIncludeTree,
            final IErlModelManager modelManager, final String rootName,
            final String rootEntry) throws ErlModelException {
        final Map<String, IErlExternal> pathToEntryMap = Maps.newHashMap();
        pathToEntryMap.put("root", this);
        for (final ExternalTreeEntry entry : externalIncludeTree) {
            final String path = entry.getPath();
            // final String name = entry.getName();
            final IErlExternal parent = pathToEntryMap.get(entry
                    .getParentPath());
            if (entry.isModule()) {
                final IPath p = new Path(path);
                final String name = p.lastSegment();
                final IErlModule module = modelManager.getModuleFromFile(
                        parent, name, null, path, path);
                parent.addChild(module);
            } else {
                final String name = getNameFromExternalPath(path);
                final ErlExternalReferenceEntry externalReferenceEntry = new ErlExternalReferenceEntry(
                        parent, name, path, true);
                pathToEntryMap.put(path, externalReferenceEntry);
                externalReferenceEntry.open(pm);
                parent.addChild(externalReferenceEntry);
            }
        }
    }

    private static String getNameFromExternalPath(String path) {
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
    protected void closing(final Object info) throws ErlModelException {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean isOpen() {
        return super.isOpen();
    }

    @Override
    public String getFilePath() {
        return null;
    }

    @Override
    public String getLabelString() {
        return getName();
    }

    public String getExternalName() {
        return externalName;
    }

    public boolean hasModuleWithPath(final String path) {
        return false;
    }

    public Backend getBackend() {
        return null;
    }

    public boolean isOTP() {
        return false;
    }

}
