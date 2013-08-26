/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.debug.model;

import java.io.IOException;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.erlide.util.ErlLogger;

public class ErtsProcess extends RuntimeProcess {

    public ErtsProcess(final ILaunch launch, final Process process,
            final String nodeName, final String workingDir) {
        super(launch, process, nodeName, null);
        ErlLogger.debug("# create ErtsProcess: " + nodeName);
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
        try {
            getLaunch().terminate();
        } catch (final DebugException e) {
            ErlLogger.error(e);
        }
        super.terminated();
    }

    @Override
    public void terminate() throws DebugException {
        ErlLogger.debug("ErtsProcess will be terminated: %s, called from: %s",
                getLabel(), new Throwable().getStackTrace()[1]);
        try {
            super.terminate();
        } finally {
            if (!isTerminated()) {
                ErlLogger.debug("Could not terminate process %s", getLabel());
            }
        }
    }
}
