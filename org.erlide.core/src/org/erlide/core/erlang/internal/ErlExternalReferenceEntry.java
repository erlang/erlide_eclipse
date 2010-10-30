package org.erlide.core.erlang.internal;

import java.io.IOException;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlExternal;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.google.common.collect.Lists;

import erlang.ErlideOpen;

public class ErlExternalReferenceEntry extends Openable implements IErlExternal {

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
        final IErlModel model = ErlangCore.getModel();
        final OtpErlangList pathVars = model.getPathVars();
        final List<String> external1 = ErlideOpen.getExternal1(backend, entry,
                pathVars);
        final List<IErlElement> children = Lists
                .newArrayListWithCapacity(external1.size());
        for (final String pathS : external1) {
            final Path path = new Path(pathS);
            String name = path.lastSegment();
            if (ErlideUtil.hasModuleExtension(name)) {
                String initialText;
                try {
                    initialText = new String(Util.getFileCharContent(pathS,
                            "UTF8"));
                } catch (final IOException e) {
                    initialText = "";
                }
                final IErlModule module = ErlangCore.getModelManager()
                        .getModuleFromFile(this, name, initialText, pathS,
                                pathS);
                children.add(module);
            } else {
                name = name.replaceAll("\\.erlidex", "");
                children.add(new ErlExternalReferenceEntry(this, name, entry));
            }
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

    public String getExternalName() {
        return entry;
    }

}
