package org.erlide.ui.console;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendListener;
import org.erlide.backend.api.IPluginCodeLoader;
import org.erlide.util.ErlLogger;
import org.erlide.util.IDisposable;

import com.google.common.collect.Maps;

public class ErlConsoleManager implements IDisposable, IBackendListener {
    private final Map<IBackend, IErlangConsole> consoles;
    private final Map<IErlangConsole, IErlangConsolePage> pages;
    private final IConsoleManager conMan;

    public ErlConsoleManager() {
        consoles = Maps.newHashMap();
        pages = Maps.newHashMap();

        final ConsolePlugin consolePlugin = ConsolePlugin.getDefault();
        conMan = consolePlugin.getConsoleManager();

        BackendCore.getBackendManager().addBackendListener(this);
    }

    @Override
    public void runtimeAdded(final IBackend b) {
        if (b == null) {
            return;
        }
        final BackendData data = b.getData();
        if (!data.hasConsole()) {
            return;
        }
        ErlLogger.debug("console ADDED to " + data);
        final ErlangConsole console = new ErlangConsole(b);
        conMan.addConsoles(new IConsole[] { console });
        consoles.put(b, console);
    }

    @Override
    public void runtimeRemoved(final IBackend b) {
        ErlLogger.debug("console REMOVED from " + b.getName());
        final IConsole console = consoles.get(b);
        if (console == null) {
            return;
        }
        conMan.removeConsoles(new IConsole[] { console });
    }

    public void addPage(final IErlangConsole console,
            final IErlangConsolePage page) {
        pages.put(console, page);
    }

    public void removePage(final IErlangConsole console) {
        pages.remove(console);
    }

    public IErlangConsolePage getPage(final IErlangConsole console) {
        return pages.get(console);
    }

    public IErlangConsole getConsole(final IPluginCodeLoader backend) {
        return consoles.get(backend);
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
