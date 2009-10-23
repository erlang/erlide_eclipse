package org.erlide.ui.console;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendListener;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.ErlideBackend;

public class ErlConsoleManager implements IDisposable, BackendListener {
	private final Map<Backend, IConsole> consoles;
	private final IConsoleManager conMan;

	public ErlConsoleManager() {
		consoles = new HashMap<Backend, IConsole>();

		ConsolePlugin consolePlugin = ConsolePlugin.getDefault();
		conMan = consolePlugin.getConsoleManager();

		ErlangCore.getBackendManager().addBackendListener(this);
	}

	public void runtimeAdded(Backend b) {
		if (b == null || !b.getInfo().hasConsole()) {
			return;
		}
		Object str = b == null ? "null" : b.getInfo();
		ErlLogger.debug("console ADDED " + b + " " + str);
		if (b instanceof ErlideBackend) {
			ErlangConsole console = new ErlangConsole((ErlideBackend) b);
			conMan.addConsoles(new IConsole[] { console });
			consoles.put(b, console);
		}
	}

	public void runtimeRemoved(Backend b) {
		ErlLogger.debug("console REMOVED " + b + " " + b.getInfo());
		IConsole console = consoles.get(b);
		if (console == null) {
			return;
		}
		conMan.removeConsoles(new IConsole[] { console });
	}

	public void dispose() {
		ErlangCore.getBackendManager().removeBackendListener(this);
	}

}
