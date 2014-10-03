package org.erlide.backend.api;

import java.io.IOException;
import java.util.Collection;

import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.IDisposable;

public interface IBackend extends IShutdownCallback, IDisposable, IPluginCodeLoader,
        IProjectCodeLoader {

    String getName();

    BackendData getData();

    IOtpRpc getOtpRpc();

    IOtpNodeProxy getRuntime();

    boolean isRunning();

    boolean isDebugging();

    IBackendShell getShell(String string);

    void initialize(CodeContext context, Collection<ICodeBundle> collection);

    void input(final String s) throws IOException;

}
