/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.jinterface.util.ErlLogger;

public class DummyProcess implements IProcess {

    private final ILaunch fLaunch;

    public DummyProcess(final ILaunch launch) {
        super();
        fLaunch = launch;
    }

    public String getLabel() {
        ErlLogger.debug("getlabel");
        return "...";
    }

    public ILaunch getLaunch() {
        return fLaunch;
    }

    public IStreamsProxy getStreamsProxy() {
        return null;
    }

    public void setAttribute(final String key, final String value) {
    }

    public String getAttribute(final String key) {
        return "?" + key;
    }

    public int getExitValue() throws DebugException {
        return 0;
    }

    public Object getAdapter(@SuppressWarnings("rawtypes") final Class adapter) {
        ErlLogger.debug("--> adapt as " + adapter.getName());
        return null;
    }

    public boolean canTerminate() {
        return false;
    }

    public boolean isTerminated() {
        return false;
    }

    public void terminate() throws DebugException {
    }

}
