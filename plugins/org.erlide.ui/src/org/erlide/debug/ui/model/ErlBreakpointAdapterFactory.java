/**
 *
 */
package org.erlide.debug.ui.model;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;
import org.eclipse.jdt.annotation.Nullable;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * @author jakob
 *
 */
public class ErlBreakpointAdapterFactory implements IAdapterFactory {

    @Override
    public <@Nullable T> @Nullable T getAdapter(final Object adaptableObject, final Class<@Nullable T> adapterType) {
        if (adaptableObject instanceof ErlangEditor) {
            final AbstractErlangEditor editorPart = (AbstractErlangEditor) adaptableObject;
            final IResource resource = editorPart.getEditorInput()
                    .getAdapter(IResource.class);
            if (resource != null) {
                final String extension = resource.getFileExtension();
                if (extension != null && "erl".equals(extension)) {
                    return adapterType.cast(new ErlLineBreakpointAdapter());
                }
            }
        }
        return null;
    }

    @Override
    public Class<?>[] getAdapterList() {
        return new Class[] { IToggleBreakpointsTarget.class };
    }

}
