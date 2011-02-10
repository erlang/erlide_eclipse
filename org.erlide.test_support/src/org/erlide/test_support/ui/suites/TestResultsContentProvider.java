package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.test_support.ui.suites.TestCaseData.TestState;

import com.ericsson.otp.erlang.OtpErlangObject;

class TestResultsContentProvider implements ITreeContentProvider {
    private static final String[] NO_RESULTS_MSG = new String[] { "No test results available." };
    private static final Object[] NO_CHILDREN = new Object[] {};

    private List<OtpErlangObject> myEvents;

    @SuppressWarnings("unchecked")
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        myEvents = (List<OtpErlangObject>) newInput;
    }

    public void dispose() {
        myEvents = null;
    }

    public Object[] getElements(final Object inputElement) {
        if (myEvents.size() == 0) {
            return NO_RESULTS_MSG;
        }
        return myEvents.toArray();
    }

    // TODO group after test suite

    public Object[] getChildren(final Object parentElement) {
        if (parentElement instanceof TestCaseData) {
            final TestCaseData data = (TestCaseData) parentElement;
            if (data.getState() == TestState.FAILED) {
                return new String[] { data.getFailLocations(),
                        data.getFailReason() };
            }
        }
        return NO_CHILDREN;
    }

    public Object getParent(final Object element) {
        return null;
    }

    public boolean hasChildren(final Object element) {
        return getChildren(element).length > 0;
    }
}