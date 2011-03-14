package org.erlide.cover.ui.views.helpers;

import java.util.Set;

import org.apache.log4j.Logger;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.ICoverageStats;
import org.erlide.cover.views.model.ObjectType;
import org.erlide.ui.editors.erl.outline.ErlangElementImageProvider;

/**
 * Label provider for statistics view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class StatsViewLabelProvider extends LabelProvider implements
        ITableLabelProvider {

    private Logger log = Logger.getLogger(getClass());

    public Image getColumnImage(final Object element, final int columnIndex) {
        Image img = null;

        final ICoverageObject statsEl = (ICoverageObject) element;

        switch (columnIndex) {
        case 0:
            ObjectType type = statsEl.getType();

            switch (type) {
            case FUNCTION:
                img = JavaUI
                        .getSharedImages()
                        .getImageDescriptor(
                                org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PRIVATE)
                        .createImage();
                break;
            case MODULE:
             /*   Set<IErlModule> mods = ErlangCore.getModuleMap()
                        .getModulesByName(statsEl.getLabel());
                IErlModule m = mods.iterator().next();
                if (m == null)
                    return null;
                img = ErlangElementImageProvider.getErlImageDescriptor(m,
                        ErlangElementImageProvider.SMALL_ICONS).createImage();*/
                //TODO
                break;
            case FOLDER:
                img = PlatformUI.getWorkbench().getSharedImages()
                        .getImageDescriptor(ISharedImages.IMG_OBJ_FOLDER)
                        .createImage();
                break;
            case PROJECT:
                img = PlatformUI.getWorkbench().getSharedImages()
                        .getImageDescriptor(IDE.SharedImages.IMG_OBJ_PROJECT)
                        .createImage();
                break;
            }
            break;
        case 3:

            img = drawPercentage(statsEl.getPercentage());
            break;
        default:
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

            text = String.format("%.2f ", statsEl.getPercentage()) + "%";
            break;
        }
        return text;
    }

    private Image drawPercentage(final double percentage) {

        final Image img = new Image(Display.getCurrent(), new Rectangle(2, 3,
                20, 8));

        final GC graphic = new GC(img);
        graphic.setForeground(new Color(Display.getCurrent(), 60, 140, 10));
        graphic.setBackground(new Color(Display.getCurrent(), 60, 140, 10));
        graphic.drawRectangle(0, 0, 18, 6);
        graphic.fillRectangle(1, 1, (int) (17 * percentage / 100), 5);

        return img;
    }

}
