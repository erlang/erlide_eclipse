package org.erlide.model.internal.erlang;

import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.model.ErlModelException;
import org.erlide.model.IParent;
import org.erlide.model.internal.root.Openable;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlExternalRoot;

public class ErlExternalReferenceEntryListProxy extends Openable implements
        IErlExternalRoot {

    private final IErlExternalRoot original;

    public ErlExternalReferenceEntryListProxy(final IParent parent,
            final String name, final IErlExternalRoot original) {
        super(parent, name);
        this.original = original;
    }

    @Override
    public String getLabelString() {
        return original.getLabelString();
    }

    @Override
    public Kind getKind() {
        return original.getKind();
    }

    @Override
    public boolean isOTP() {
        return original.isOTP();
    }

    @Override
    public boolean hasIncludes() {
        return original.hasIncludes();
    }

    @Override
    public List<IErlElement> internalGetChildren() {
        return original.internalGetChildren();
    }

    @Override
    public boolean buildStructure(final IProgressMonitor pm)
            throws ErlModelException {
        return ((Openable) original).buildStructure(pm);
    }
}
