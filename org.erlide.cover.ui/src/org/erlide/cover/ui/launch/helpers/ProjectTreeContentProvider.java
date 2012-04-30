package org.erlide.cover.ui.launch.helpers;

import java.util.Collection;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.root.IErlProject;

/**
 * Content provider for projects
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class ProjectTreeContentProvider implements ITreeContentProvider {

    private Collection<IErlProject> projects;

    @Override
    public void dispose() {
        projects = null;
    }

    @Override
    @SuppressWarnings("unchecked")
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        projects = (Collection<IErlProject>) newInput;
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return projects.toArray();
    }

    @Override
    public Object[] getChildren(final Object parentElement) {
        return null;
    }

    @Override
    public Object getParent(final Object element) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean hasChildren(final Object element) {
        // TODO Auto-generated method stub
        return false;
    }

}
