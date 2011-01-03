package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.cover.views.model.ICoverageStats;

public class StatsViewLabelProvider extends LabelProvider
		implements ITableLabelProvider
		{

/*	public String getText(Object obj) {
		return obj.toString();
	}
	public Image getImage(Object obj) {
		String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
		if (obj instanceof StatsTreeParent)
		   imageKey = ISharedImages.IMG_OBJ_FOLDER;
		return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
	}*/
	
	public Image getColumnImage(Object element, int columnIndex) {
		//no image
		return null;
	}
	

	public String getColumnText(Object element, int columnIndex) {
		ICoverageStats statsEl = (ICoverageStats)element;
		String text = "";
		
		switch(columnIndex) {
		case 0: 
			text = statsEl.getLabel(); 
			break;
		case 1: 
			text = Integer.toString(statsEl.getLinesCount()); 
			break;
		case 2:
			text = Integer.toString(statsEl.getCoverCount()); 
			break;
		case 3:
			text = Double.toString(statsEl.getPrecentage()); 
			break;
		}
		return text;
	}
	
}
