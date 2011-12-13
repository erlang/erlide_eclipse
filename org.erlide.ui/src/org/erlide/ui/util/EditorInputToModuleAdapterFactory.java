package org.erlide.ui.util;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.ui.IEditorInput;
import org.erlide.core.model.root.IErlElement;

public class EditorInputToModuleAdapterFactory implements IAdapterFactory {

    @SuppressWarnings("rawtypes")
    private static final Class[] ADAPTER_LIST = new Class[] { IErlElement.class };

    @Override
    @SuppressWarnings("rawtypes")
    public Object getAdapter(final Object adaptableObject,
            final Class adapterType) {
        if (adapterType == IErlElement.class
                && adaptableObject instanceof IEditorInput) {
            try {
                return ErlModelUtils.getModule((IEditorInput) adaptableObject);
            } catch (final CoreException e) {
            }
        }
        return null;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public Class[] getAdapterList() {
        return ADAPTER_LIST;
    }

}
