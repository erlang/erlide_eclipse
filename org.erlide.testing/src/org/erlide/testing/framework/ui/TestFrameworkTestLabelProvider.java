package org.erlide.testing.framework.ui;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.erlide.testing.framework.model.TestFrameworkTestElement;


public class TestFrameworkTestLabelProvider extends LabelProvider {

	@Override
	public String getText(Object obj) {
		return obj.toString();
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof TestFrameworkTestElement) {
			TestFrameworkTestElement testCaseElement = ((TestFrameworkTestElement) element);

			switch (testCaseElement.getStatus()) {
			case TestFrameworkTestElement.STATUS_FAILED:
				return TestFrameworkTestRunnerViewPart.fTestFailIcon;
			case TestFrameworkTestElement.STATUS_NOT_RUN:
				return TestFrameworkTestRunnerViewPart.fTestIcon;
			case TestFrameworkTestElement.STATUS_OK:
				return TestFrameworkTestRunnerViewPart.fTestOkIcon;
			case TestFrameworkTestElement.STATUS_RUNNING:
				return TestFrameworkTestRunnerViewPart.fTestRunningIcon;
			}
		}
		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
	}
}
