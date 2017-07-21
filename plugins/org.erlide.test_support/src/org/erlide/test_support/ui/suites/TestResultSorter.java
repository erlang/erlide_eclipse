package org.erlide.test_support.ui.suites;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

class TestResultSorter extends ViewerSorter {
    @Override
    public int compare(final Viewer viewer, final Object e1, final Object e2) {
        if (e1 instanceof TestCaseData && e2 instanceof TestCaseData) {
            final TestCaseData item1 = (TestCaseData) e1;
            final TestCaseData item2 = (TestCaseData) e2;
            return item2.getState().compareTo(item1.getState());
        }
        return 0;
    }
}
