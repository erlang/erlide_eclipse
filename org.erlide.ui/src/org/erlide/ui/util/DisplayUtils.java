package org.erlide.ui.util;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;

public final class DisplayUtils {

    public static void asyncExec(final Runnable runnable) {
        final Display display = PlatformUI.getWorkbench().getDisplay();
        if (display != null && !display.isDisposed()) {
            display.asyncExec(runnable);
        }
    }

    public static void syncExec(final Runnable runnable) {
        if (Display.getCurrent() == null) {
            final Display display = PlatformUI.getWorkbench().getDisplay();
            if (display != null && !display.isDisposed()) {
                display.syncExec(runnable);
            }
        } else {
            runnable.run();
        }
    }

    private DisplayUtils() {
    }
}
