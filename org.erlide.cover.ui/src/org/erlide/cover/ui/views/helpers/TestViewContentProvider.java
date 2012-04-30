package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IViewSite;
import org.erlide.cover.views.model.TestTreeModel;
import org.erlide.cover.views.model.TestTreeObject;

/**
 * Content provider for the EUnit test view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class TestViewContentProvider implements ITreeContentProvider,
        IStructuredContentProvider {

    private final IViewSite viewSite;
    private TestTreeModel model;

    public TestViewContentProvider(final IViewSite viewSite) {
        this.viewSite = viewSite;
    }

    @Override
    public void dispose() {
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        if (newInput instanceof TestTreeModel) {
            model = (TestTreeModel) newInput;
        }
    }

    @Override
    public Object[] getElements(final Object parent) {
        if (parent.equals(viewSite) && model != null || parent.equals(model)) {
            return model.getRootLevel().toArray();
        }

        return getChildren(parent);
    }

    @Override
    public Object[] getChildren(final Object parent) {
        if (parent instanceof TestTreeObject
                && ((TestTreeObject) parent).hasChildren()) {
            return ((TestTreeObject) parent).getChildren().toArray();
        }
        return new Object[0];
    }

    @Override
    public Object getParent(final Object child) {
        if (child instanceof TestTreeObject) {
            return ((TestTreeObject) child).getParent();
        }
        return null;
    }

    @Override
    public boolean hasChildren(final Object parent) {
        if (parent instanceof TestTreeObject) {
            return ((TestTreeObject) parent).hasChildren();
        }
        return false;
    }

}
