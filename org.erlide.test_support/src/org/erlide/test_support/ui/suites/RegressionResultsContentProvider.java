package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

public class RegressionResultsContentProvider implements
        IStructuredContentProvider {

    private List<String> data;

    @SuppressWarnings("unchecked")
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        data = (List<String>) newInput;
    }

    public Object[] getElements(final Object inputElement) {
        return data.toArray();
    }

    public void dispose() {
    }

}
