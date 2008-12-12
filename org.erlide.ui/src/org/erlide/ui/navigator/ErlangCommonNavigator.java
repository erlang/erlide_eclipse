package org.erlide.ui.navigator;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.navigator.CommonNavigator;
import org.eclipse.ui.navigator.CommonViewer;

public class ErlangCommonNavigator extends CommonNavigator {

	@Override
	public void selectReveal(final ISelection selection) {
		final CommonViewer cv = getCommonViewer();
		if (cv != null) {
			// if(selection instanceof IStructuredSelection) {
			// Object[] newSelection =
			// ((IStructuredSelection)selection).toArray();
			// Object[] expandedElements = cv.getExpandedElements();
			// Object[] newExpandedElements = new Object[newSelection.length +
			// expandedElements.length];
			// System.arraycopy(expandedElements, 0, newExpandedElements, 0,
			// expandedElements.length);
			// System.arraycopy(newSelection, 0, newExpandedElements,
			// expandedElements.length, newSelection.length);
			// cv.setExpandedElements(newExpandedElements);
			// }
			cv.setSelection(selection, true);
		}
	}

}
