package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IViewSite;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * Content provider for statistics view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class StatsViewContentProvider implements IStructuredContentProvider,
        ITreeContentProvider {

    private final IViewSite viewSite;
    private StatsTreeModel model;

    public StatsViewContentProvider(final IViewSite viewSite) {
        this.viewSite = viewSite;
    }

    @Override
    public void inputChanged(final Viewer v, final Object oldInput,
            final Object newInput) {
        if (newInput instanceof StatsTreeModel) {
            model = (StatsTreeModel) newInput;
        }

    }

    @Override
    public void dispose() {

    }

    @Override
    public Object[] getElements(final Object parent) {
        if (parent.equals(viewSite) && model != null || parent.equals(model)) {
            return new ICoverageObject[] { model.getRoot() };
        }

        return getChildren(parent);
    }

    @Override
    public Object getParent(final Object child) {
        if (child instanceof ICoverageObject) {
            return ((ICoverageObject) child).getParent();
        }
        return null;
    }

    @Override
    public Object[] getChildren(final Object parent) {
        if (parent instanceof ICoverageObject
                && ((ICoverageObject) parent).hasChildren()) {
            return ((ICoverageObject) parent).getChildren();
        }
        return new Object[0];
    }

    @Override
    public boolean hasChildren(final Object parent) {
        if (parent instanceof ICoverageObject) {
            return ((ICoverageObject) parent).hasChildren();
        }
        return false;
    }

}
