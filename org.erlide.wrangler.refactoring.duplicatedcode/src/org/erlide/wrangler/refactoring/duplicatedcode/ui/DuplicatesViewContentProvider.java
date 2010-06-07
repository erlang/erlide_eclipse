/**
 * 
 */
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.wrangler.refactoring.duplicatedcode.DuplicatesUIManager;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.AbstractResultTreeObject;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.AbstractResultTreeParent;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.DuplicatedCodeElement;
import org.erlide.wrangler.refactoring.duplicatedcode.ui.elements.ResultTreeRoot;

/**
 * Content provider for the duplicates view.
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicatesViewContentProvider implements
		IStructuredContentProvider, ITreeContentProvider,
		IDuplicatedCodeResultDisplayer {

	private final DuplicatesView duplicatedCodeView;

	DuplicatesViewContentProvider(DuplicatesView duplicatedCodeView) {
		this.duplicatedCodeView = duplicatedCodeView;
		DuplicatesUIManager.setDuplicatedCodeResultDisplayer(this);
	}

	private ResultTreeRoot invisibleRoot;

	public void inputChanged(Viewer v, Object oldInput, Object newInput) {
	}

	public void dispose() {
	}

	public Object[] getElements(Object parent) {
		if (parent.equals(this.duplicatedCodeView.getViewSite())) {
			if (invisibleRoot == null)
				initialize();
			return getChildren(invisibleRoot);
		}
		return getChildren(parent);
	}

	public Object getParent(Object child) {
		if (child instanceof AbstractResultTreeObject) {
			return ((AbstractResultTreeObject) child).getParent();
		}
		return null;
	}

	public Object[] getChildren(Object parent) {
		if (parent instanceof AbstractResultTreeParent) {
			return ((AbstractResultTreeParent) parent).getChildren();
		}
		return new Object[0];
	}

	public boolean hasChildren(Object parent) {
		if (parent instanceof AbstractResultTreeParent)
			return ((AbstractResultTreeParent) parent).hasChildren();
		return false;
	}

	private void initialize() {
		invisibleRoot = new ResultTreeRoot();
	}

	/**
	 * Add a child to the view.
	 * 
	 * @param child
	 *            child to be added.
	 */
	public void addChild(AbstractResultTreeObject child) {
		invisibleRoot.addChild(child);
	}

	public void showResult(List<DuplicatedCodeElement> result) {
		invisibleRoot.dropChildren();
		if (result != null)
			for (DuplicatedCodeElement d : result) {
				invisibleRoot.addChild(d);
			}
		this.duplicatedCodeView.refresh();

	}
}