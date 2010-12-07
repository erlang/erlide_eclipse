package org.erlide.cover.ui.launch.helpers;

import java.util.Collection;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlProject;

public class ProjectTreeContentProvider implements ITreeContentProvider {

    private Collection<IErlProject> projects;
    
    public void dispose() {
        // TODO Auto-generated method stub
        
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        projects = (Collection)newInput;        
    }

    public Object[] getElements(Object inputElement) {
        return projects.toArray();
    }

    public Object[] getChildren(Object parentElement) {
        return null;
    }

    public Object getParent(Object element) {
        // TODO Auto-generated method stub
        return null;
    }

    public boolean hasChildren(Object element) {
        // TODO Auto-generated method stub
        return false;
    }

}
