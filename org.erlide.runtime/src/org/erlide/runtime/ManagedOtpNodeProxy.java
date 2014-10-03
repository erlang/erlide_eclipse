/*******************************************************************************
 * Copyright (c) 2010 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Map;

import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErlRuntimeException;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

public class ManagedOtpNodeProxy extends OtpNodeProxy {

    private Process process;
    private volatile int exitCode;

    private static final long EXIT_POLL_INTERVAL = 500;

    public ManagedOtpNodeProxy(final RuntimeData data) {
        super(data);
    }

    @Override
    protected void startUp() throws Exception {
        exitCode = -1;
        process = startRuntimeProcess(data);
        if (process == null) {
            throw new Exception("no runtime");
        }
        super.startUp();
    }

    @Override
    protected void shutDown() throws Exception {
        super.shutDown();
        process.destroy();
        process = null;
    }

    @Override
    public Process getProcess() {
        awaitRunning();
        return process;
    }

    private Process startRuntimeProcess(final RuntimeData rtData) {
        final String[] cmds = rtData.getCmdLine();
        final File workingDirectory = new File(rtData.getWorkingDir());

        try {
            ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                    + workingDirectory.getCanonicalPath());
        } catch (final IOException e1) {
            ErlLogger.error("START ERROR node :> " + e1.getMessage());
        }

        final ProcessBuilder builder = new ProcessBuilder(cmds);
        builder.directory(workingDirectory);
        setEnvironment(rtData, builder);
        try {
            final Process aProcess = builder.start();
            return aProcess;
        } catch (final IOException e) {
            ErlLogger.error("Could not create runtime: %s", Arrays.toString(cmds));
            ErlLogger.error(e);
            return null;
        }
    }

    private void setEnvironment(final RuntimeData data, final ProcessBuilder builder) {
        final Map<String, String> env = builder.environment();
        if (!SystemConfiguration.getInstance().isOnWindows()
                && SystemConfiguration.getInstance().hasSpecialTclLib()) {
            env.put("TCL_LIBRARY", "/usr/share/tcl/tcl8.4/");
        }
        if (data.getEnv() != null) {
            env.putAll(data.getEnv());
        }
    }

    @Override
    protected void waitForExit() throws ErlRuntimeException {
        if (process != null) {
            int i = 500;
            // may have to wait for crash dump to be written
            while (i-- > 0 && exitCode < 0) {
                exitCode = -1;
                try {
                    Thread.sleep(EXIT_POLL_INTERVAL);
                    exitCode = process.exitValue();
                } catch (final IllegalThreadStateException e) {
                } catch (final InterruptedException e) {
                }
                if (exitCode > 0) {
                    throw new ErlRuntimeException(String.format(
                            "Runtime %s died with exit code %d", getNodeName(), exitCode));
                }
            }
            if (exitCode < 0) {
                ErlLogger.warn(
                        "Runtime %s died, but process is still running; killing it",
                        getNodeName());
                throw new ErlRuntimeException(String.format(
                        "Runtime %s died with exit code unknown", getNodeName()));
            }
        }
    }

    @Override
    public int getExitCode() {
        return exitCode;
    }
}
