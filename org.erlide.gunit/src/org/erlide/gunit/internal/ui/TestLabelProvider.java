package org.erlide.gunit.internal.ui;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.erlide.gunit.model.TestElement;


public class TestLabelProvider extends LabelProvider {

	@Override
	public String getText(Object obj) {
		return obj.toString();
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof TestElement) {
			TestElement testCaseElement = ((TestElement) element);

			switch (testCaseElement.getStatus()) {
			case TestElement.STATUS_FAILED:
				return TestRunnerViewPart.fTestFailIcon;
			case TestElement.STATUS_NOT_RUN:
				return TestRunnerViewPart.fTestIcon;
			case TestElement.STATUS_OK:
				return TestRunnerViewPart.fTestOkIcon;
			case TestElement.STATUS_RUNNING:
				return TestRunnerViewPart.fTestRunningIcon;
			}
		}
		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
	}
}
