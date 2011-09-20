package org.erlide.test_support.ui.suites;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

public class RegressionResultsLabelProvider implements ITableLabelProvider {

    public void addListener(final ILabelProviderListener listener) {
    }

    public void dispose() {
    }

    public boolean isLabelProperty(final Object element, final String property) {
        return true;
    }

    public void removeListener(final ILabelProviderListener listener) {
    }

    public Image getColumnImage(final Object element, final int columnIndex) {
        return null;
    }

    public String getColumnText(final Object element, final int columnIndex) {
        return element.toString();
    }

}
