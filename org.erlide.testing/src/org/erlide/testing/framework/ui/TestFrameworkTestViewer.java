package org.erlide.testing.framework.ui;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public class TestFrameworkTestViewer {

	private TreeViewer fTreeViewer;
	private TestFrameworkTestTreeContentProvider fTreeContentProvider;
	private TestFrameworkTestLabelProvider fTreeLabelProvider;

	public TestFrameworkTestViewer(Composite parent) {
		fTreeViewer = new TreeViewer(parent, SWT.V_SCROLL | SWT.SINGLE);
		fTreeContentProvider = new TestFrameworkTestTreeContentProvider();
		fTreeViewer.setContentProvider(fTreeContentProvider);
		fTreeLabelProvider = new TestFrameworkTestLabelProvider();
		fTreeViewer.setLabelProvider(fTreeLabelProvider);
	}

	public void setInput(Object input) {
		fTreeViewer.setInput(input);
	}

	public void refresh() {
		fTreeViewer.refresh();
	}

	public Control getControl() {
		return fTreeViewer.getControl();
	}

}
