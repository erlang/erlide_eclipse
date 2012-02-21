/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.launch.debug.model;

import java.io.IOException;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErtsWatcher;

public class ErtsProcess extends RuntimeProcess {

    public ErtsProcess(final ILaunch launch, final Process process,
            final String nodeName, final String workingDir) {
        super(launch, process, nodeName, null);
        ErlLogger.debug("# create ErtsProcess: " + nodeName);

        startWatcher(nodeName, workingDir, process);
    }

    /**
     * @return Returns the started.
     */
    public boolean isStarted() {
        return getLaunch() != null;
    }

    /**
     * Write something out to the node process.
     * 
     * @param value
     *            The system.
     * @throws IOException
     */
    public synchronized void writeToErlang(final String value)
            throws IOException {
        if (!isStarted()) {
            return;
        }
        final IStreamsProxy astreamsProxy = getStreamsProxy();
        if (astreamsProxy != null) {
            astreamsProxy.write(value);
        }
    }

    /**
     * if this isn't already stopped, try to stop it.
     * 
     * @throws Throwable
     */
    @Override
    protected void finalize() throws Throwable {
        terminate();
        super.finalize();
    }

    public void addStdListener(final IStreamListener dspHandler) {
        final IStreamsProxy streamsProxy = getStreamsProxy();
        if (streamsProxy != null) {
            streamsProxy.getOutputStreamMonitor().addListener(dspHandler);
        }
    }

    public void addErrListener(final IStreamListener errHandler) {
        final IStreamsProxy streamsProxy = getStreamsProxy();
        if (streamsProxy != null) {
            streamsProxy.getErrorStreamMonitor().addListener(errHandler);
        }
    }

    @Override
    protected void terminated() {
        ErlLogger.debug("ErtsProcess terminated: %s", getLabel());
        super.terminated();
        try {
            getLaunch().terminate();
        } catch (final DebugException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void terminate() throws DebugException {
        ErlLogger.debug("ErtsProcess will be terminated: %s", getLabel());
        super.terminate();
    }

    private void startWatcher(final String nodeName, final String workingDir,
            final Process process) {
        final Runnable watcher = new ErtsWatcher(nodeName, workingDir, process);
        final Thread thread = new Thread(null, watcher, "ErtsProcess watcher");
        thread.setDaemon(true);
        thread.setPriority(Thread.MIN_PRIORITY);
        thread.start();
    }

}
