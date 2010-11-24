package org.erlide.eunit.ui.views.helpers;

import java.util.LinkedList;
import java.util.List;

/*
 * The content provider class is responsible for
 * providing objects to the view. It can wrap
 * existing objects in adapters or simply return
 * objects as-is. These objects may be sensitive
 * to the current input of the view, or ignore
 * it and always show the same content 
 * (like Task List, for example).
 */
public class StatsTreeObject implements IStatsTreeObject{

	private String label;
	private int all;
	private int covered;
	private int percentage;
	
	private IStatsTreeObject parent;
	private List<IStatsTreeObject> children;
		
	public StatsTreeObject(){
		children = new LinkedList<IStatsTreeObject>();
	}
	
	public StatsTreeObject(IStatsTreeObject parent){
		this();
		this.parent = parent;
	}
	
	public StatsTreeObject(String label, int all, int covered, int percentage) {
		this();
		this.label = label;
		this.all = all;
		this.covered = covered;
		this.percentage = percentage;
	}
	
	public String getLabel() {
		return label;
	}
	
	public String toString() {
		return String.format("%s %d %d %d\n", label, all, covered, percentage);
	}
	
	public Object getAdapter(Class key) {
		return null;
	}

	public void setParent(IStatsTreeObject parent) {
		this.parent = parent;
	}
	
	@Override
	public IStatsTreeObject getParent() {
		return parent;
	}

	public void addChild(IStatsTreeObject child) {
		children.add(child);
		child.setParent(this);
	}

	public void removeChild(IStatsTreeObject child) {
		children.remove(child);
		child.setParent(null);
	}

	public IStatsTreeObject[] getChildren() {
		return children.toArray(new IStatsTreeObject[0]);
	}

	public boolean hasChildren() {
		return !children.isEmpty();
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public int getLinesCount() {
		return all;
	}

	public void setLiniesCount(int count) {
		all = count;
	}

	public int getCoverCount() {
		return covered;
	}

	public void setCoverCount(int count) {
		covered = count;
	}

	public int getPrecentage() {
		return percentage;
	}

	public void setPercentage(int count) {
		percentage = count;
	}

	@Override
	public void removeAllChildren() {
		children.clear();
	}

	
}
