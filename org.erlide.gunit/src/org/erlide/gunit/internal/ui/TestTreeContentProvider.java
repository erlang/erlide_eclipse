package org.erlide.gunit.internal.ui;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.gunit.model.TestElement;


public class TestTreeContentProvider implements ITreeContentProvider {

	public void dispose() {
	}

	public Object[] getChildren(Object parentElement) {
		TestElement testElement = (TestElement) parentElement;
		return testElement.getChildren().toArray();
	}

	public Object[] getElements(Object inputElement) {
		return ((List<?>) inputElement).toArray();
	}

	public Object getParent(Object element) {
		TestElement testElement = (TestElement) element;
		return testElement.getParent();
	}

	public boolean hasChildren(Object element) {
		TestElement testElement = (TestElement) element;
		return testElement.hasChildren();
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
}
