package org.erlide.core.erlang.internal;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ModelUtils;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

public class ErlExternalReferenceEntry extends Openable implements IErlElement,
        IParent {

    private final String entry;

    protected ErlExternalReferenceEntry(final IErlElement parent,
            final String name, final String entry) {
        super(parent, name);
        this.entry = entry;
    }

    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        ErlLogger.debug("ErlExternalReferenceEntry.buildStructure");
        final Backend backend = BackendUtils
                .getBuildOrIdeBackend(getErlProject().getProject());
        final List<String> externalModules = ModelUtils.getExternalModules(
                backend, "", ErlangCore.getModel(), entry);
        final List<IErlElement> children = new ArrayList<IErlElement>(
                externalModules.size());
        for (final String path : externalModules) {
            String initialText;
            try {
                initialText = new String(Util.getFileCharContent(path, "UTF8"));
            } catch (final IOException e) {
                initialText = "";
            }
            final String name = new Path(path).lastSegment();
            final IErlModule module = ErlangCore.getModelManager()
                    .getModuleFromFile(this, name, initialText, path, path);
            children.add(module);
        }
        setChildren(children);
        return true;
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

}
