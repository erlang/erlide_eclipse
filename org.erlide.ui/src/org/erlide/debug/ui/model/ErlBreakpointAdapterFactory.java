/**
 *
 */
package org.erlide.debug.ui.model;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * @author jakob
 *
 */
public class ErlBreakpointAdapterFactory implements IAdapterFactory {

    @Override
    public Object getAdapter(final Object adaptableObject, final Class adapterType) {
        if (adaptableObject instanceof ErlangEditor) {
            final AbstractErlangEditor editorPart = (AbstractErlangEditor) adaptableObject;
            final IResource resource = (IResource) editorPart.getEditorInput()
                    .getAdapter(IResource.class);
            if (resource != null) {
                final String extension = resource.getFileExtension();
                if (extension != null && "erl".equals(extension)) {
                    return new ErlLineBreakpointAdapter();
                }
            }
        }
        return null;
    }

    @Override
    public Class[] getAdapterList() {
        return new Class[] { IToggleBreakpointsTarget.class };
    }

}
