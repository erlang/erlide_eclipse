package org.erlide.test_support.ui.suites;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.ui.ErlideImage;

class TestResultsLabelProvider extends LabelProvider {
    @Override
    public Image getImage(final Object element) {
        if (element instanceof TestCaseData) {
            final TestCaseData data = (TestCaseData) element;
            switch (data.getState()) {
            case NOT_RUN:
                return null;
            case SKIPPED:
                return ErlideImage.TEST_SKIPPED.getImage();
            case RUNNING:
                return ErlideImage.TEST_RUNNING.getImage();
            case SUCCESS:
                return ErlideImage.TEST_SUCCEEDED.getImage();
            case FAILED:
                return ErlideImage.TEST_FAILED.getImage();
            }
        }
        return null;
    }

    @Override
    public String getText(final Object element) {
        if (element instanceof TestCaseData) {
            final TestCaseData data = (TestCaseData) element;
            return data.getModule() + " : " + data.getFunction();
        }
        return element.toString();
    }
}
