package org.erlide.ui.tests.util;

import org.eclipse.ui.PlatformUI;

public class WorkbenchHelper {

    public static void waitForWorkbench() {
        int i = 0;
        while (!PlatformUI.isWorkbenchRunning() && i < 50) {
            try {
                i++;
                Thread.sleep(50);
            } catch (final InterruptedException e) {
                // ignore
            }
        }
    }
}
