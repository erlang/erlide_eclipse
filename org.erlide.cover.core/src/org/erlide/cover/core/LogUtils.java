package org.erlide.cover.core;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;

public class LogUtils {
    
    public static void log(Plugin plugin, int severity, Object obj, int depth) {
        if (severity < IStatus.INFO)
            return;
        String pluginId = plugin.getClass().getPackage().getName();
        String msg = String.valueOf(obj) + stackInfo(depth + 3);
        Status status = new Status(severity, pluginId, msg);
        plugin.getLog().log(status);
    }


    private static String stackInfo(int depth) {
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        if (stackTrace.length <= depth)
            return "";
        StackTraceElement caller = stackTrace[depth];
        return String.format("\n%s.%s(...)\n%s:%s", caller.getClassName(), caller.getMethodName(),
                                                     caller.getFileName(),  caller.getLineNumber());
    }

}
