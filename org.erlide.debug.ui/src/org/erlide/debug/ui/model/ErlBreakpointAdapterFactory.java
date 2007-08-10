/**
 * 
 */
package org.erlide.debug.ui.model;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.debug.ui.actions.IToggleBreakpointsTarget;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * @author jakob
 * 
 */
public class ErlBreakpointAdapterFactory implements IAdapterFactory {

	@SuppressWarnings("unchecked")
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (adaptableObject instanceof ErlangEditor) {
			ErlangEditor editorPart = (ErlangEditor) adaptableObject;
			IResource resource = (IResource) editorPart.getEditorInput()
					.getAdapter(IResource.class);
			if (resource != null) {
				String extension = resource.getFileExtension();
				if (extension != null && "erl".equals(extension)) {
					return new ErlLineBreakpointAdapter();
				}
			}
		}
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.IAdapterFactory#getAdapterList()
	 */
	@SuppressWarnings("unchecked")
	public Class[] getAdapterList() {
		return new Class[] { IToggleBreakpointsTarget.class };
	}

}