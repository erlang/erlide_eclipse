package org.erlide.test_support.ui;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.debug.ui.actions.ILaunchable;

public class ErlElementAdapterFactory implements IAdapterFactory {

    @Override
    @SuppressWarnings("rawtypes")
    public Object getAdapter(final Object adaptableObject,
            final Class adapterType) {
        // we don't use this, just need it for menu enablement
        return null;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public Class[] getAdapterList() {
        return new Class[] { ILaunchable.class };
    }

}
