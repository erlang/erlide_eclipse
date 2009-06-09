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
package org.erlide.runtime.backend;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.internal.CodeManager;
import org.osgi.framework.Bundle;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class ErlideBackend extends Backend implements IDisposable {

	private final CodeManager codeManager;

	public ErlideBackend(final RuntimeInfo info) throws BackendException {
		super(info);
		codeManager = new CodeManager(this);
	}

	@Override
	public void dispose() {
		dispose(false);
	}

	@Override
	public void dispose(final boolean restart) {
		ErlLogger.debug("disposing backend " + getName());
		super.dispose(restart);
	}

	@Override
	public void initializeRuntime() {
		super.initializeRuntime();
	}

	@Override
	public synchronized void restart() {
		super.restart();
		codeManager.reRegisterBundles();
		// initErlang();
		// fixme eventdaemon
	}

	public void removePath(final String path) {
		codeManager.removePath(path);
	}

	public void addPath(final boolean usePathZ, final String path) {
		codeManager.addPath(usePathZ, path);
	}

	@Override
	public void initErlang() {
		super.initErlang();

	}

	public void register(Bundle bundle) {
		codeManager.register(bundle);
	}

	public void unregister(Bundle b) {
		codeManager.unregister(b);
	}

	public void setTrapExit(boolean contains) {
		// TODO Auto-generated method stub

	}

}
