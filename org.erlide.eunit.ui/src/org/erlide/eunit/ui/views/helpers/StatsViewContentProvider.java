package org.erlide.eunit.ui.views.helpers;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IViewSite;

public class StatsViewContentProvider implements IStructuredContentProvider, 
ITreeContentProvider {
	
	private IViewSite viewSite;
	private IStatsTreeObject invisibleRoot;
	
	public StatsViewContentProvider(IViewSite viewSite){
		this.viewSite = viewSite;
		initialize();
	}

	public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		if(newInput instanceof IStatsTreeObject){
			invisibleRoot.removeAllChildren();
			invisibleRoot.addChild((IStatsTreeObject)newInput);
		}
		/*	if(newInput instanceof Collection<?>){
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
		if (parent.equals(viewSite)) {
			return getChildren(invisibleRoot);
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
/*
* We will set up a dummy model to initialize tree heararchy.
* In a real code, you will connect to a real model and
* expose its hierarchy.
*/
	private void initialize() {
		
		invisibleRoot = new StatsTreeObject();
		
		StatsTreeObject root = new StatsTreeObject("total", 0, 0, 0);
		invisibleRoot.addChild(root);
		
		for(int i = 1; i < 5; i++) {
			IStatsTreeObject module = new StatsTreeObject("module" +i, 12+i, 10, 52);
			root.addChild(module);
			for(int j = 1; j < 3; j++) {
				IStatsTreeObject function = new StatsTreeObject("function"+j, 4+j, 2, 10*j);
				module.addChild(function);
			}
		}
		
	}
}
