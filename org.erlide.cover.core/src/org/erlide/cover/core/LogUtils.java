package org.erlide.cover.core;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;

/**
 * Log class
 * 
 * @author Krzysztof Goj
 * 
 */
public class LogUtils {

    public static void log(final Plugin plugin, final int severity,
            final Object obj, final int depth) {
        if (severity < IStatus.WARNING) {
            return;
        }
        final String pluginId = plugin.getClass().getPackage().getName();
        final String msg = String.valueOf(obj) + stackInfo(depth + 3);
        final Status status = new Status(severity, pluginId, msg);
        plugin.getLog().log(status);
    }

    private static String stackInfo(final int depth) {
        final StackTraceElement[] stackTrace = Thread.currentThread()
                .getStackTrace();
        if (stackTrace.length <= depth) {
            return "";
        }
        final StackTraceElement caller = stackTrace[depth];
        return String.format("\n%s.%s(...)\n%s:%s", caller.getClassName(),
                caller.getMethodName(), caller.getFileName(),
                caller.getLineNumber());
    }

}
