package org.erlide.ui.navigator;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.navigator.IDescriptionProvider;

public class ErlangFileLabelProvider extends LabelProvider implements
		ILabelProvider, IDescriptionProvider {

	public Image getImage(Object element) {
		if (element instanceof ErlangFileTreeData) {
			return PlatformUI.getWorkbench().getSharedImages().getImage(
					ISharedImages.IMG_OBJS_INFO_TSK);

		}
		return null;
	}

	public String getText(Object element) {
		if (element instanceof ErlangFileTreeData) {
			ErlangFileTreeData data = (ErlangFileTreeData) element;
			return data.getName();// + "= " + data.getValue(); //$NON-NLS-1$
		}
		return null;
	}

	public String getDescription(Object anElement) {
		if (anElement instanceof ErlangFileTreeData) {
			ErlangFileTreeData data = (ErlangFileTreeData) anElement;
			return "Property: " + data.getName(); //$NON-NLS-1$
		}
		return null;
	}

	public void addListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public boolean isLabelProperty(Object element, String property) {
		// TODO Auto-generated method stub
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		// TODO Auto-generated method stub

	}

}
