package org.erlide.test_support.ui.suites;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.console.ConsolePluginImages;
import org.eclipse.ui.internal.console.IInternalConsoleConstants;

class TestResultsLabelProvider extends LabelProvider {
    @Override
    public Image getImage(final Object element) {
        if (element instanceof TestCaseData) {
            final TestCaseData data = (TestCaseData) element;
            switch (data.getState()) {
            case NOT_RUN:
                return ConsolePluginImages
                        .getImage(IInternalConsoleConstants.IMG_ELCL_CLEAR);
            case RUNNING:
                return PlatformUI.getWorkbench().getSharedImages()
                        .getImage(ISharedImages.IMG_TOOL_REDO);
            case SUCCESS:
                return PlatformUI.getWorkbench().getSharedImages()
                        .getImage(ISharedImages.IMG_OBJS_INFO_TSK);
            case FAILURE:
                return PlatformUI.getWorkbench().getSharedImages()
                        .getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
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