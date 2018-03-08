package org.erlide.core.internal;

import java.io.File;
import java.util.logging.Level;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.erlide.core.ErlangCore;
import org.erlide.core.ErlangStatus;
import org.erlide.util.ErlLogger;

public class ErlangCoreLogger {
    private final Plugin plugin;
    private final ErlLogger logger;

    public ErlangCoreLogger(final Plugin plugin) {
        this.plugin = plugin;
        logger = ErlLogger.getInstance();
        final String logFile = ErlangCoreLogger.getLogFile();
        log(Level.INFO, "Erlide log is in " + logFile);
        logger.setLogFile(logFile);
    }

    public void debug(final String message) {
        if (plugin.isDebugging()) {
            ErlLogger.debug(message);
        }
    }

    public void log(final IStatus status) {
        final Level lvl = ErlangCoreLogger.getLevelFromSeverity(status.getSeverity());
        logger.log(lvl, status.getMessage());
        final Throwable exception = status.getException();
        if (exception != null) {
            logger.log(lvl, exception);
        }
        plugin.getLog().log(status);
    }

    public void log(final Level lvl, final String status) {
        logger.log(lvl, status);
        plugin.getLog().log(new Status(ErlangCoreLogger.getSeverityFromLevel(lvl),
                plugin.getBundle().getSymbolicName(), status));
    }

    public void log(final String msg, final Throwable thr) {
        final String id = plugin.getBundle().getSymbolicName();
        final Status status = new Status(IStatus.ERROR, id, IStatus.OK, msg, thr);
        plugin.getLog().log(status);
    }

    public void log(final Throwable e) {
        log(new Status(IStatus.ERROR, plugin.getBundle().getSymbolicName(),
                ErlangStatus.INTERNAL_ERROR.getValue(), "Erlide internal error", e));
    }

    public void logErrorMessage(final String message) {
        log(new Status(IStatus.ERROR, plugin.getBundle().getSymbolicName(),
                ErlangStatus.INTERNAL_ERROR.getValue(), message, null));
    }

    public void logErrorStatus(final String message, final IStatus status) {
        if (status == null) {
            logErrorMessage(message);
            return;
        }
        final MultiStatus multi = new MultiStatus(plugin.getBundle().getSymbolicName(),
                ErlangStatus.INTERNAL_ERROR.getValue(), message, null);
        multi.add(status);
        log(multi);
    }

    public static String getLogFile() {
        final IPreferencesService service = Platform.getPreferencesService();
        final String pluginId = ErlangCore.PLUGIN_ID;
        String file = System.getProperty("erlide.log_file");
        if (file == null) {
            file = service.getString(pluginId, "log_file", "erlide.log", null);
        }
        final IPath path = new Path(file);
        String location;
        if (!path.isAbsolute()) {
            final IPath wroot = ResourcesPlugin.getWorkspace().getRoot().getLocation();
            location = wroot.append(file).toPortableString();
        } else {
            location = file;
        }
        new File(location).getParentFile().mkdirs();
        return location;
    }

    private static Level getLevelFromSeverity(final int severity) {
        Level lvl;
        switch (severity) {
        case IStatus.ERROR:
            lvl = Level.SEVERE;
            break;
        case IStatus.WARNING:
            lvl = Level.WARNING;
            break;
        case IStatus.INFO:
            lvl = Level.INFO;
            break;
        default:
            lvl = Level.FINEST;
        }
        return lvl;
    }

    private static int getSeverityFromLevel(final Level lvl) {
        final int sev;
        if (lvl == Level.SEVERE) {
            sev = IStatus.ERROR;
        } else if (lvl == Level.WARNING) {
            sev = IStatus.WARNING;
        } else if (lvl == Level.INFO) {
            sev = IStatus.INFO;
        } else {
            sev = IStatus.OK;
        }
        return sev;
    }

}
