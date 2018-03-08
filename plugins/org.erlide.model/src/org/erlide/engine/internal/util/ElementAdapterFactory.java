package org.erlide.engine.internal.util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.jdt.annotation.Nullable;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IErlElement;

public class ElementAdapterFactory implements IAdapterFactory {

    @SuppressWarnings("rawtypes")
    private static final Class[] ADAPTER_LIST = { IErlElement.class };

    @Override
    public <T> @Nullable T getAdapter(final Object adaptableObject,
            final Class<T> adapterType) {
        if (adapterType == IErlElement.class && adaptableObject instanceof IResource) {
            return adapterType.cast(ErlangEngine.getInstance().getModel()
                    .findElement((IResource) adaptableObject));
        }
        return null;
    }

    @Override
    public Class<?>[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
