package org.erlide.ui.console;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendData;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendListener;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.IDisposable;

public class ErlConsoleManager implements IDisposable, IBackendListener {
    private final Map<IBackend, IConsole> consoles;
    private final IConsoleManager conMan;

    public ErlConsoleManager() {
        consoles = new HashMap<IBackend, IConsole>();

        final ConsolePlugin consolePlugin = ConsolePlugin.getDefault();
        conMan = consolePlugin.getConsoleManager();

        BackendCore.getBackendManager().addBackendListener(this);
    }

    @Override
    public void runtimeAdded(final IBackend b) {
        if (b == null) {
            return;
        }
        final BackendData info = b.getData();
        if (!info.hasConsole()) {
            return;
        }
        ErlLogger.debug("console ADDED to " + info);
        final ErlangConsole console = new ErlangConsole(b);
        conMan.addConsoles(new IConsole[] { console });
        consoles.put(b, console);
    }

    @Override
    public void runtimeRemoved(final IBackend b) {
        ErlLogger.debug("console REMOVED from " + b.getRuntimeInfo());
        final IConsole console = consoles.get(b);
        if (console == null) {
            return;
        }
        conMan.removeConsoles(new IConsole[] { console });
    }

    @Override
    public void dispose() {
        BackendCore.getBackendManager().removeBackendListener(this);
    }

    @Override
    public void moduleLoaded(final IBackend backend, final IProject project,
            final String moduleName) {
    }
}
