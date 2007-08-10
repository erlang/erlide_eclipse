/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.debug;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.basiccore.ErlLogger;

public class DummyProcess implements IProcess {

	private ILaunch fLaunch;

	public DummyProcess(ILaunch launch) {
		super();
		fLaunch = launch;
	}

	public String getLabel() {
		ErlLogger.log("getlabel");
		return "...";
	}

	public ILaunch getLaunch() {
		return fLaunch;
	}

	public IStreamsProxy getStreamsProxy() {
		return null;
	}

	public void setAttribute(String key, String value) {
	}

	public String getAttribute(String key) {
		return "?" + key;
	}

	public int getExitValue() throws DebugException {
		return 0;
	}

	@SuppressWarnings("unchecked")
	public Object getAdapter(Class adapter) {
		ErlLogger.log("--> adapt as " + adapter.getName());
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
