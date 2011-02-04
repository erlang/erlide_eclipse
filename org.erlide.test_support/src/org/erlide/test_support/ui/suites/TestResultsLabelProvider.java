package org.erlide.test_support.ui.suites;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.ui.ErlideUIPluginImages;

class TestResultsLabelProvider extends LabelProvider {
    @SuppressWarnings("restriction")
    @Override
    public Image getImage(final Object element) {
        if (element instanceof TestCaseData) {
            final TestCaseData data = (TestCaseData) element;
            switch (data.getState()) {
            case NOT_RUN:
                return null;
            case SKIPPED:
                return ErlideUIPluginImages
                        .get(ErlideUIPluginImages.IMG_TEST_SKIPPED);
            case RUNNING:
                return ErlideUIPluginImages
                        .get(ErlideUIPluginImages.IMG_TEST_RUNNING);
            case SUCCESS:
                return ErlideUIPluginImages
                        .get(ErlideUIPluginImages.IMG_TEST_SUCCESS);
            case FAILURE:
                return ErlideUIPluginImages
                        .get(ErlideUIPluginImages.IMG_TEST_FAILED);
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