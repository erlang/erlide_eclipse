package org.erlide.engine.internal.util;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.jdt.annotation.Nullable;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlProject;

@SuppressWarnings("rawtypes")
public class ProjectAdapterFactory implements IAdapterFactory {

    private static final Class[] ADAPTER_LIST = {
            IErlProject.class
    };

    @Override
    public <T> @Nullable T getAdapter(final Object adaptableObject,
            final Class<T> adapterType) {
        if (adapterType == IErlProject.class && adaptableObject instanceof IProject) {
            return adapterType.cast(ErlangEngine.getInstance().getModel()
                    .getErlangProject((IProject) adaptableObject));
        }
        return null;
    }

    @Override
    public Class<?>[] getAdapterList() {
        return ProjectAdapterFactory.ADAPTER_LIST;
    }

}
