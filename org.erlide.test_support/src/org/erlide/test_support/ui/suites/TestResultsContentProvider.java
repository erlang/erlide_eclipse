package org.erlide.test_support.ui.suites;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.test_support.ui.suites.TestCaseData.FailLocations;
import org.erlide.test_support.ui.suites.TestCaseData.FailReason;
import org.erlide.test_support.ui.suites.TestCaseData.TestState;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

class TestResultsContentProvider implements ITreeContentProvider {
    private static final String[] NO_RESULTS_MSG = new String[] { "No test results available." };
    private static final Object[] NO_CHILDREN = new Object[] {};

    private List<OtpErlangObject> myEvents;

    @Override
    @SuppressWarnings("unchecked")
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        myEvents = (List<OtpErlangObject>) newInput;
    }

    @Override
    public void dispose() {
        myEvents = null;
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        if (myEvents.size() == 0) {
            return NO_RESULTS_MSG;
        }
        return myEvents.toArray();
    }

    // TODO group after test suite

    @Override
    public Object[] getChildren(final Object parentElement) {
        if (parentElement instanceof TestCaseData) {
            final TestCaseData data = (TestCaseData) parentElement;
            if (data.getState() == TestState.FAILED) {
                final List<Object> result = Lists.newArrayList();
                result.add(data.getFailStack());
                if (!data.getFailLocations().isEmpty()) {
                    result.add(data.getFailLocations());
                }
                return result.toArray();
            }
        } else if (parentElement instanceof FailLocations) {
            final FailLocations locs = (FailLocations) parentElement;
            return locs.getLocations().toArray();
        } else if (parentElement instanceof FailReason) {
            final FailReason stack = (FailReason) parentElement;
            return stack.getStackItems().toArray();
        }
        return NO_CHILDREN;
    }

    @Override
    public Object getParent(final Object element) {
        return null;
    }

    @Override
    public boolean hasChildren(final Object element) {
        return getChildren(element).length > 0;
    }
}
