package org.erlide.ui.console;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.IBackendListener;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.common.IDisposable;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;

public class ErlConsoleManager implements IDisposable, IBackendListener {
    private final Map<IBackend, IConsole> consoles;
    private final IConsoleManager conMan;

    public ErlConsoleManager() {
        consoles = new HashMap<IBackend, IConsole>();

        final ConsolePlugin consolePlugin = ConsolePlugin.getDefault();
        conMan = consolePlugin.getConsoleManager();

        BackendCore.getBackendManager().addBackendListener(this);
    }

    public void runtimeAdded(final IBackend b) {
        if (b == null || !b.getRuntimeInfo().hasConsole()) {
            return;
        }
        final RuntimeInfo info = b.getRuntimeInfo();
        ErlLogger.debug("console ADDED to " + info);
        final ErlangConsole console = new ErlangConsole(b);
        conMan.addConsoles(new IConsole[] { console });
        consoles.put(b, console);
    }

    public void runtimeRemoved(final IBackend b) {
        ErlLogger.debug("console REMOVED from " + b.getRuntimeInfo());
        final IConsole console = consoles.get(b);
        if (console == null) {
            return;
        }
        conMan.removeConsoles(new IConsole[] { console });
    }

    public void dispose() {
        BackendCore.getBackendManager().removeBackendListener(this);
    }

    public void moduleLoaded(final IRpcCallSite backend,
            final IProject project, final String moduleName) {
    }
}
