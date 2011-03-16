package org.erlide.cover.ui;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.statushandlers.StatusManager;

/**
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class CoverageHelper {

    public static void reportError(String info) {
        final IStatus executionStatus = new Status(IStatus.ERROR,
                Activator.PLUGIN_ID, info, null);
        StatusManager.getManager().handle(executionStatus,
                StatusManager.SHOW);
    }
    
    public static void reportInfo(String info) {
        final IStatus executionStatus = new Status(IStatus.INFO,
                Activator.PLUGIN_ID, info, null);
        StatusManager.getManager().handle(executionStatus,
                StatusManager.SHOW);
    }
    
}
