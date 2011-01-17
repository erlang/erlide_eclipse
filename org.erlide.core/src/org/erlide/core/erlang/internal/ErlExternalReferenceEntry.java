package org.erlide.core.erlang.internal;

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlModelManager;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Lists;

import erlang.ErlideOpen;

public class ErlExternalReferenceEntry extends Openable implements IErlExternal {

    private final String entry;
    private final boolean isRoot;

    protected ErlExternalReferenceEntry(final IParent parent,
            final String name, final String entry, final boolean isRoot) {
        super(parent, name);
        this.entry = entry;
        this.isRoot = isRoot;
    }

    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        final Backend backend = BackendUtils
                .getBuildOrIdeBackend(getErlProject().getProject());
        final OtpErlangList pathVars = ErlangCore.getModel().getPathVars();
        final List<IErlElement> children = getEntryChildren(this, entry,
                isRoot, backend, pathVars, pm);
        setChildren(children);
        return true;
    }

    public static List<IErlElement> getEntryChildren(final IParent parent,
            final String fileName, final boolean isRoot, final Backend backend,
            final OtpErlangList pathVars, final IProgressMonitor pm)
            throws ErlModelException {
        ErlLogger.debug("reading external %s", fileName);
        final List<String> external1 = ErlideOpen.getExternal1(backend,
                fileName, pathVars, isRoot);
        final List<IErlElement> children = Lists
                .newArrayListWithCapacity(external1.size());
        final IErlModelManager modelManager = ErlangCore.getModelManager();
        for (final String path : external1) {
            final String name = getNameFromExternalPath(path);
            if (ErlideUtil.hasModuleExtension(path)) {
                final IErlModule module = modelManager.getModuleFromFile(
                        parent, name, null, path, path);
                children.add(module);
            } else if (ErlideUtil.hasErlideExternalExtension(path) || isRoot) {
                final ErlExternalReferenceEntry child = new ErlExternalReferenceEntry(
                        parent, name, path, false);
                children.add(child);
                child.open(pm);
            }
        }
        return children;
    }

    private static String getNameFromExternalPath(String path) {
        int i = path.indexOf(".settings");
        if (i > 2) {
            path = path.substring(0, i - 1);
        }
        i = path.lastIndexOf('/');
        return path.substring(i + 1).replace("\\.erlidex", "");
    }

    @Override
    protected void closing(final Object info) throws ErlModelException {
        // TODO Auto-generated method stub
    }

    @Override
    public String getFilePath() {
        return null;
    }

    @Override
    public String getLabelString() {
        return super.getName();
    }

    public String getExternalName() {
        return entry;
    }

}
