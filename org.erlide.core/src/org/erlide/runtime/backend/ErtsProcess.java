/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.RuntimeProcess;
import org.erlide.jinterface.util.ErlLogger;

public class ErtsProcess extends RuntimeProcess {

    public static final String CONFIGURATION_TYPE = "org.erlide.core.launch.erlangProcess";
    public static final String CONFIGURATION_TYPE_INTERNAL = "org.erlide.core.launch.internal";

    public ErtsProcess(final ILaunch launch, final Process process,
            final String name,
            @SuppressWarnings("rawtypes") final Map attributes) {
        super(launch, process, name, attributes);
        // ErlLogger.debug("# create ErtsNode: " + name + " " + attributes);
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

    @SuppressWarnings("unused")
    private String[] mkEnv(final Map<String, String> map) {
        final Set<Map.Entry<String, String>> entries = map.entrySet();
        final String[] result = new String[entries.size()];
        final Iterator<Map.Entry<String, String>> i = entries.iterator();
        int ii = 0;
        while (i.hasNext()) {
            final Map.Entry<String, String> e = i.next();
            result[ii++] = e.getKey() + "=" + e.getValue();
        }
        return result;
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

    public void sendToShell(final String string) {
        final IStreamsProxy streamsProxy = getStreamsProxy();
        if (streamsProxy != null) {
            try {
                streamsProxy.write(string);
                ErlLogger.debug("#>>#" + string);
            } catch (final IOException e) {
                ErlLogger.warn(e);
            }
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

}
