package org.erlide.testing.framework.ui;

import java.util.List;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.testing.framework.model.TestFrameworkTestElement;


public class TestFrameworkTestTreeContentProvider implements ITreeContentProvider {

	public void dispose() {
	}

	public Object[] getChildren(Object parentElement) {
		TestFrameworkTestElement testElement = (TestFrameworkTestElement) parentElement;
		return testElement.getChildren().toArray();
	}

	public Object[] getElements(Object inputElement) {
		return ((List<?>) inputElement).toArray();
	}

	public Object getParent(Object element) {
		TestFrameworkTestElement testElement = (TestFrameworkTestElement) element;
		return testElement.getParent();
	}

	public boolean hasChildren(Object element) {
		TestFrameworkTestElement testElement = (TestFrameworkTestElement) element;
		return testElement.hasChildren();
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
	}
}
