package org.erlide.branding;

import org.eclipse.core.runtime.IProduct;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.branding.IProductConstants;
import org.eclipse.ui.splash.BasicSplashHandler;
import org.erlide.core.ErlangPlugin;

public class SplashHandler extends BasicSplashHandler {

    @Override
    public void init(final Shell splash) {
        super.init(splash);
        String progressRectString = null;
        String messageRectString = null;
        String foregroundColorString = null;
        final IProduct product = Platform.getProduct();
        if (product != null) {
            progressRectString = product
                    .getProperty(IProductConstants.STARTUP_PROGRESS_RECT);
            messageRectString = product
                    .getProperty(IProductConstants.STARTUP_MESSAGE_RECT);
            foregroundColorString = product
                    .getProperty(IProductConstants.STARTUP_FOREGROUND_COLOR);
        }
        final Rectangle progressRect = StringConverter.asRectangle(progressRectString,
                new Rectangle(10, 10, 300, 15));
        setProgressRect(progressRect);

        final Rectangle messageRect = StringConverter.asRectangle(messageRectString,
                new Rectangle(10, 35, 300, 15));
        setMessageRect(messageRect);

        int foregroundColorInteger;
        try {
            foregroundColorInteger = Integer.parseInt(foregroundColorString, 16);
        } catch (final Exception ex) {
            foregroundColorInteger = 0xD2D7FF; // off white
        }

        setForeground(new RGB((foregroundColorInteger & 0xFF0000) >> 16,
                (foregroundColorInteger & 0xFF00) >> 8, foregroundColorInteger & 0xFF));

        final String buildId = ErlangPlugin.getDefault().getCore().getFeatureVersion();
        final Point buildIdPoint;
        // hardcoded to be sensible with our current splash Graphic
        if (product != null) {
            final String buildIdLocString = product.getProperty("buildIdLocation"); //$NON-NLS-1$
            buildIdPoint = StringConverter.asPoint(buildIdLocString,
                    new Point(30, splash.getSize().y - 60));
        } else {
            buildIdPoint = new Point(30, splash.getSize().y - 60);
        }

        getContent().addPaintListener(new PaintListener() {

            @Override
            public void paintControl(final PaintEvent e) {
                e.gc.setForeground(getForeground());
                e.gc.drawText(buildId, buildIdPoint.x, buildIdPoint.y, true);
            }
        });
    }
}
