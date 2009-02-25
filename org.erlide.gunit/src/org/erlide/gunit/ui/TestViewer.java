package org.erlide.gunit.ui;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public class TestViewer {

	private TreeViewer fTreeViewer;
	private TestTreeContentProvider fTreeContentProvider;
	private TestLabelProvider fTreeLabelProvider;

	public TestViewer(Composite parent) {
		fTreeViewer = new TreeViewer(parent, SWT.V_SCROLL | SWT.SINGLE);
		fTreeContentProvider = new TestTreeContentProvider();
		fTreeViewer.setContentProvider(fTreeContentProvider);
		fTreeLabelProvider = new TestLabelProvider();
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
