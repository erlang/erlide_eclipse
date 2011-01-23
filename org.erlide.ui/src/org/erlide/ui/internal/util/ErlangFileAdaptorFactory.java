package org.erlide.ui.internal.util;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.core.runtime.content.IContentTypeManager;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.ui.editors.erl.ErlangEditor;

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
                final IContentTypeManager contentTypeManager = Platform
                        .getContentTypeManager();
                final IContentType[] contentTypes = contentTypeManager
                        .findContentTypesFor(file.getName());
                for (final IContentType contentType : contentTypes) {
                    if (contentType.getId().equals(
                            "org.erlide.core.content.erlang")) {
                        return ErlangCore.getModel().findModule(file);
                    }
                }
                final IEditorRegistry editorRegistry = PlatformUI
                        .getWorkbench().getEditorRegistry();
                if (editorRegistry.getDefaultEditor(file.getName()).getId()
                        .equals(ErlangEditor.ERLANG_EDITOR_ID)) {
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
