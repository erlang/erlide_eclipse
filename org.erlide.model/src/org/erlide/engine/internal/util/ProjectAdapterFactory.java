package org.erlide.engine.internal.util;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdapterFactory;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlProject;

@SuppressWarnings("rawtypes")
public class ProjectAdapterFactory implements IAdapterFactory {

    private static final Class[] ADAPTER_LIST = new Class[] { IErlProject.class };

    @Override
    public Object getAdapter(final Object adaptableObject, final Class adapterType) {
        if (adapterType == IErlProject.class && adaptableObject instanceof IProject) {
            return ErlangEngine.getInstance().getModel()
                    .getErlangProject((IProject) adaptableObject);
        }
        return null;
    }

    @Override
    public Class[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
