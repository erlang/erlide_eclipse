package org.erlide.model.util;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdapterFactory;
import org.erlide.model.root.ErlModelManager;
import org.erlide.model.root.IErlProject;

@SuppressWarnings("rawtypes")
public class ProjectAdapterFactory implements IAdapterFactory {

    private static final Class[] ADAPTER_LIST = new Class[] { IErlProject.class };

    @Override
    public Object getAdapter(final Object adaptableObject,
            final Class adapterType) {
        if (adapterType == IErlProject.class
                && adaptableObject instanceof IProject) {
            return ErlModelManager.getErlangModel().getErlangProject(
                    (IProject) adaptableObject);
        }
        return null;
    }

    @Override
    public Class[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
