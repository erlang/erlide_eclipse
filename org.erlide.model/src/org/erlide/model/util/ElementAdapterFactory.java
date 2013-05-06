package org.erlide.model.util;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.erlide.model.root.ErlModelManager;
import org.erlide.model.root.IErlElement;

public class ElementAdapterFactory implements IAdapterFactory {

    @SuppressWarnings("rawtypes")
    private static final Class[] ADAPTER_LIST = new Class[] { IErlElement.class };

    @Override
    public Object getAdapter(final Object adaptableObject,
            final Class adapterType) {
        if (adapterType == IErlElement.class
                && adaptableObject instanceof IResource) {
            return ErlModelManager.getErlangModel().findElement(
                    (IResource) adaptableObject);
        }
        return null;
    }

    @Override
    public Class[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
