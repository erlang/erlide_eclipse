package org.erlide.eunit.ui.views.helpers;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IViewSite;
import org.erlide.eunit.views.model.IStatsTreeObject;
import org.erlide.eunit.views.model.StatsTreeModel;

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
		
		/*if(newInput instanceof IStatsTreeObject){
			invisibleRoot.removeAllChildren();
			invisibleRoot.addChild((IStatsTreeObject)newInput);
		}
			if(newInput instanceof Collection<?>){
			root.removeAllChildren();
			Collection<IStatsTreeObject> newChildren = (Collection<IStatsTreeObject>)newInput;
			for(IStatsTreeObject child : newChildren) {
				root.addChild(child);
			}
		}*/
	}
	
	public void dispose() {
	
	}
	
	public Object[] getElements(Object parent) {
		if (parent.equals(viewSite) && model != null) {
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
