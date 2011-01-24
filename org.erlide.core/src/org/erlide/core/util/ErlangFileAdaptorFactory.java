package org.erlide.core.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdapterFactory;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.ErlideUtil;

public class ErlangFileAdaptorFactory implements IAdapterFactory {

    public ErlangFileAdaptorFactory() {
        super();
        // TODO Auto-generated constructor stub
    }

    @SuppressWarnings("rawtypes")
    public Object getAdapter(final Object adaptableObject,
            final Class adapterType) {
        if (adapterType == IErlModule.class) {
            if (adaptableObject instanceof IFile) {
                final IFile file = (IFile) adaptableObject;
                final String fileName = file.getName();
                if (ErlideUtil.isErlangFileContentFileName(fileName)) {
                    return ErlangCore.getModel().findModule(file);
                }
            }
        }
        return null;
    }

    @SuppressWarnings("rawtypes")
    public Class[] getAdapterList() {
        return new Class[] { IErlModule.class };
    }

}
