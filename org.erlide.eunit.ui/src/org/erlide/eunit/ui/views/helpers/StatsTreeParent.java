package org.erlide.eunit.ui.views.helpers;

import java.util.ArrayList;

public class StatsTreeParent extends StatsTreeObject{
	
	private ArrayList children;
	public StatsTreeParent(String name) {

		children = new ArrayList();
	}
	public void addChild(StatsTreeObject child) {
		children.add(child);
		child.setParent(this);
	}
	public void removeChild(StatsTreeObject child) {
		children.remove(child);
		child.setParent(null);
	}
	public StatsTreeObject [] getChildren() {
		return (StatsTreeObject [])children.toArray(new StatsTreeObject[children.size()]);
	}
	public boolean hasChildren() {
		return children.size()>0;
	}

}
