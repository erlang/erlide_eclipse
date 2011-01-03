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
	
	

}
