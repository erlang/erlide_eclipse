package org.erlide.test_support.ui.suites;

import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;

public class RegressionResultsLabelProvider implements ITableLabelProvider {

    @Override
    public void addListener(final ILabelProviderListener listener) {
    }

    @Override
    public void dispose() {
    }

    @Override
    public boolean isLabelProperty(final Object element, final String property) {
        return true;
    }

    @Override
    public void removeListener(final ILabelProviderListener listener) {
    }

    @Override
    public Image getColumnImage(final Object element, final int columnIndex) {
        return null;
    }

    @Override
    public String getColumnText(final Object element, final int columnIndex) {
        return element.toString();
    }

}
