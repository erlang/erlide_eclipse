package org.erlide.cover.ui.views.helpers;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IViewSite;
import org.erlide.cover.views.model.IStatsTreeObject;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * Content provider for statistics view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
public class StatsViewContentProvider implements IStructuredContentProvider, 
ITreeContentProvider {
	
	private IViewSite viewSite;
	private StatsTreeModel model;
	
	public StatsViewContentProvider(IViewSite viewSite){
		this.viewSite = viewSite;
	}

	public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		if(newInput instanceof StatsTreeModel) {
			this.model = (StatsTreeModel)newInput;
		}
		
	}
	
	public void dispose() {
	
	}
	
	public Object[] getElements(Object parent) {
		if ( (parent.equals(viewSite) && model != null) ||
				parent.equals(model)) {
			return new IStatsTreeObject[] {model.getRoot()};
		}
		
		return getChildren(parent);
	}
	
	public Object getParent(Object child) {
		if (child instanceof IStatsTreeObject) {
			return ((IStatsTreeObject)child).getParent();
		}
		return null;
	}
	
	public Object [] getChildren(Object parent) {
		if (parent instanceof IStatsTreeObject &&
				((IStatsTreeObject)parent).hasChildren()) {
			return ((IStatsTreeObject)parent).getChildren();
		}
		return new Object[0];
	}
	
	public boolean hasChildren(Object parent) {
		if (parent instanceof IStatsTreeObject)
			return ((IStatsTreeObject)parent).hasChildren();
		return false;
	}
	
}
