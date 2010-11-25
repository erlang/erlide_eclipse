package org.erlide.core.erlang.internal;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Lists;

import erlang.ErlideOpen;

public class ErlExternalReferenceEntry extends Openable implements IErlExternal {

    private final String entry;
    private final boolean isRoot;

    protected ErlExternalReferenceEntry(final IErlElement parent,
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
                isRoot, backend, pathVars);
        setChildren(children);
        return true;
    }

    public static List<IErlElement> getEntryChildren(final IErlElement parent,
            final String fileName, final boolean isRoot, final Backend backend,
            final OtpErlangList pathVars) {
        final List<String> external1 = ErlideOpen.getExternal1(backend,
                fileName, pathVars, isRoot);
        final List<IErlElement> children = Lists
                .newArrayListWithCapacity(external1.size());
        for (final String path : external1) {
            final String name = getNameFromExternalPath(path);
            if (ErlideUtil.hasModuleExtension(path)) {
                String initialText;
                try {
                    initialText = new String(Util.getFileCharContent(path,
                            "UTF8"));
                } catch (final IOException e) {
                    initialText = "";
                }
                final IErlModule module = ErlangCore.getModelManager()
                        .getModuleFromFile(parent, name, initialText, path,
                                path);
                children.add(module);
            } else if (ErlideUtil.hasErlideExternalExtension(path)) {
                children.add(new ErlExternalReferenceEntry(parent, name, path,
                        false));
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
