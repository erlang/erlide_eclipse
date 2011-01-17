package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
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
		if(columnIndex == 3){
		    ICoverageStats statsEl = (ICoverageStats)element;
		    return drawPercentage(statsEl.getPrecentage());
		}
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
			text =  String.format("%.2f", statsEl.getPrecentage()); 
			break;
		}
		return text;
	}
	
	private Image drawPercentage(double percentage) {
	    
	    Image img = new Image(Display.getCurrent(), new Rectangle(2, 2, 85, 15));
	
	    GC graphic = new GC(img);
	    graphic.setForeground(new Color(Display.getCurrent(), 30, 120, 230));
	    graphic.setBackground(new Color(Display.getCurrent(), 30, 120, 230));
	    graphic.drawRectangle(2, 2, 80, 10);
	    graphic.fillRectangle(2, 2, (int)(80*percentage/100), 10);
	    
	    return img;
	}
	
}
