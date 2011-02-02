package org.erlide.cover.views.model;

import java.util.HashMap;
import java.util.Map;



/*
 * The content provider class is responsible for
 * providing objects to the view. It can wrap
 * existing objects in adapters or simply return
 * objects as-is. These objects may be sensitive
 * to the current input of the view, or ignore
 * it and always show the same content 
 * (like Task List, for example).
 */
public class StatsTreeObject implements 
        IStatsTreeObject, ICoverageStats {

	private String label;          // name
	private int all;               // total line number
	private int covered;           // covered line number
	private double percentage;     // percentage
    private String htmlPath;            //name of html file
	
	private IStatsTreeObject parent;
	private Map<String, StatsTreeObject> children;
		
	public StatsTreeObject(){
		children = new HashMap<String, StatsTreeObject>();
	}
	
	public StatsTreeObject(IStatsTreeObject parent){
		this();
		this.parent = parent;
	}
	
	public StatsTreeObject(String label, int all, int covered, double percentage) {
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
		
		StringBuffer bf = new StringBuffer();
		bf.append(label).append(" ").
			append(all).append(" ").
			append(covered).append(" ").
			append(percentage).append('\n');
		
		for(IStatsTreeObject child : children.values())
			bf.append('\t').append(child.toString()).append('\n');
		
		return bf.toString();
	}
	
	public Object getAdapter(Class key) {
		return null;
	}

	public void setParent(IStatsTreeObject parent) {
		this.parent = parent;
	}
	
	public IStatsTreeObject getParent() {
		return parent;
	}

	public void addChild(String name, IStatsTreeObject child) {
	    if(child instanceof StatsTreeObject) {
	        children.put(name, (StatsTreeObject)child);
	        child.setParent(this);
	    }
	}

	public void removeChild(String name) {
		children.remove(name);
	}

	public IStatsTreeObject[] getChildren() {
		return children.values().toArray(new IStatsTreeObject[0]);
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

	public double getPrecentage() {
		return percentage;
	}

	public void setPercentage(double count) {
		percentage = count;
	}

	public void removeAllChildren() {
		children.clear();
	}

	public String[] getStringArray() {
		return new String[] {label, Integer.toString(all),
				Integer.toString(covered), String.format("%.2f", percentage)};
	}

	public String getHtmlPath() {
        return htmlPath;
    }
    
    public void setHtmlPath(String htmlPath) {
        this.htmlPath = htmlPath;
    }

    public ICoverageStats findChild(String name) {
        return children.get(name);
    }
	
}
