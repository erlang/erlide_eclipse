package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IParent;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;

public class ErlExternalReferenceEntryList extends Openable implements
        IErlElement, IParent {

    List<String> entries;

    public ErlExternalReferenceEntryList(final IErlElement parent,
            final String name, final String externalIncludes,
            final String externalModules) {
        super(parent, name);
        entries = new ArrayList<String>();
        final List<String> modules = PreferencesUtils
                .unpackList(externalModules);
        entries.addAll(modules);
        final List<String> includes = PreferencesUtils
                .unpackList(externalIncludes);
        entries.addAll(includes);
    }

    public Kind getKind() {
        return Kind.EXTERNAL;
    }

    @Override
    protected boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        ErlLogger.debug("ErlExternalReferenceEntryList.buildStructure");
        final List<IErlElement> result = new ArrayList<IErlElement>(
                entries.size());
        for (final String entry : entries) {
            result.add(new ErlExternalReferenceEntry(this, entry));
        }
        setChildren(result);
        return true;
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

}
