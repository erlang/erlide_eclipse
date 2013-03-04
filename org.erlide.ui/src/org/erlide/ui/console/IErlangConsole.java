package org.erlide.ui.console;

import org.eclipse.ui.console.IConsole;
import org.erlide.backend.IBackend;
import org.erlide.runtime.shell.IBackendShell;

public interface IErlangConsole extends IConsole {

    public abstract IBackendShell getShell();

    public abstract IBackend getBackend();

}
