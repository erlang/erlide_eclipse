package org.erlide.cover.views.model;

import org.eclipse.core.runtime.IAdaptable;

/**
 * Interface implemented by each element displayed in coverage
 * statistics viewer
 * 
 * @author Aleksandra Lipiec
 *
 */
public interface IStatsTreeObject extends IAdaptable {
	
	public IStatsTreeObject getParent();
	public void setParent(IStatsTreeObject parent);
	
	public void addChild(IStatsTreeObject child);
	public void removeChild(IStatsTreeObject child);
	public void removeAllChildren();
	public IStatsTreeObject [] getChildren();
	public boolean hasChildren();
	
	public String getLabel();
	public void setLabel(String label);
	
	public int getLinesCount();
	public void setLiniesCount(int count);
	
	public int getCoverCount();
	public void setCoverCount(int count);
	
	public double getPrecentage();
	public void setPercentage(double count);
	
	public String[] getStringRepr();

}
