package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.cover.views.model.ICoverageStats;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Label provider for statistics view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class StatsViewLabelProvider extends LabelProvider implements
        ITableLabelProvider {

    public Image getColumnImage(final Object element, final int columnIndex) {
        Image img;
        switch (columnIndex) {
        case 0:
            if (element instanceof ModuleStats) {

                /*
                 * IErlModule m = ErlangCore.getModel().
                 * findModule(((ModuleStats)element).getLabel()); img =
                 * ErlangElementImageProvider .getErlImageDescriptor(m,
                 * ErlangElementImageProvider.SMALL_ICONS). createImage();
                 */
                // TODO: correct the image size
                img = null;

            } else {
                img = null;
            }
            break;
        case 3:
            final ICoverageStats statsEl = (ICoverageStats) element;
            img = drawPercentage(statsEl.getPrecentage());
            break;
        default:
            img = null;
        }

        return img;
    }

    public String getColumnText(final Object element, final int columnIndex) {
        final ICoverageStats statsEl = (ICoverageStats) element;
        String text = "";

        switch (columnIndex) {
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
            text = String.format("%.2f", statsEl.getPrecentage());
            break;
        }
        return text;
    }

    private Image drawPercentage(final double percentage) {

        final Image img = new Image(Display.getCurrent(), new Rectangle(2, 2,
                85, 15));

        final GC graphic = new GC(img);
        graphic.setForeground(new Color(Display.getCurrent(), 60, 140, 10));
        graphic.setBackground(new Color(Display.getCurrent(), 60, 140, 10));
        graphic.drawRectangle(2, 2, 80, 10);
        graphic.fillRectangle(2, 2, (int) (80 * percentage / 100), 10);

        return img;
    }

}
