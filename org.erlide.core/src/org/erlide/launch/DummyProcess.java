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
package org.erlide.launch;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.jinterface.ErlLogger;

public class DummyProcess implements IProcess {

    private final ILaunch fLaunch;

    public DummyProcess(final ILaunch launch) {
        super();
        fLaunch = launch;
    }

    @Override
    public String getLabel() {
        ErlLogger.debug("getlabel");
        return "...";
    }

    @Override
    public ILaunch getLaunch() {
        return fLaunch;
    }

    @Override
    public IStreamsProxy getStreamsProxy() {
        return null;
    }

    @Override
    public void setAttribute(final String key, final String value) {
    }

    @Override
    public String getAttribute(final String key) {
        return "?" + key;
    }

    @Override
    public int getExitValue() throws DebugException {
        return 0;
    }

    @Override
    public Object getAdapter(@SuppressWarnings("rawtypes") final Class adapter) {
        ErlLogger.debug("--> adapt as " + adapter.getName());
        return null;
    }

    @Override
    public boolean canTerminate() {
        return false;
    }

    @Override
    public boolean isTerminated() {
        return false;
    }

    @Override
    public void terminate() throws DebugException {
    }

}
