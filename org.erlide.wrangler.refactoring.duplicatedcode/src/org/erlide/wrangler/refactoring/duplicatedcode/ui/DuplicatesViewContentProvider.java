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

    DuplicatesViewContentProvider(final DuplicatesView duplicatedCodeView) {
        this.duplicatedCodeView = duplicatedCodeView;
        DuplicatesUIManager.setDuplicatedCodeResultDisplayer(this);
    }

    private ResultTreeRoot invisibleRoot;

    @Override
    public void inputChanged(final Viewer v, final Object oldInput,
            final Object newInput) {
    }

    @Override
    public void dispose() {
    }

    @Override
    public Object[] getElements(final Object parent) {
        if (parent.equals(duplicatedCodeView.getViewSite())) {
            if (invisibleRoot == null) {
                initialize();
            }
            return getChildren(invisibleRoot);
        }
        return getChildren(parent);
    }

    @Override
    public Object getParent(final Object child) {
        if (child instanceof AbstractResultTreeObject) {
            return ((AbstractResultTreeObject) child).getParent();
        }
        return null;
    }

    @Override
    public Object[] getChildren(final Object parent) {
        if (parent instanceof AbstractResultTreeParent) {
            return ((AbstractResultTreeParent) parent).getChildren();
        }
        return new Object[0];
    }

    @Override
    public boolean hasChildren(final Object parent) {
        if (parent instanceof AbstractResultTreeParent) {
            return ((AbstractResultTreeParent) parent).hasChildren();
        }
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
    public void addChild(final AbstractResultTreeObject child) {
        invisibleRoot.addChild(child);
    }

    @Override
    public void showResult(final List<DuplicatedCodeElement> result) {
        invisibleRoot.dropChildren();
        if (result != null) {
            for (final DuplicatedCodeElement d : result) {
                invisibleRoot.addChild(d);
            }
        }
        duplicatedCodeView.refresh();

    }
}
