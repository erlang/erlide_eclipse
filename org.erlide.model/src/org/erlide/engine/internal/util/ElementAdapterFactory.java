package org.erlide.engine.internal.util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlElement;

public class ElementAdapterFactory implements IAdapterFactory {

    @SuppressWarnings("rawtypes")
    private static final Class[] ADAPTER_LIST = new Class[] { IErlElement.class };

    @Override
    public Object getAdapter(final Object adaptableObject,
            @SuppressWarnings("rawtypes") final Class adapterType) {
        if (adapterType == IErlElement.class && adaptableObject instanceof IResource) {
            return ErlangEngine.getInstance().getModel()
                    .findElement((IResource) adaptableObject);
        }
        return null;
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Class[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
