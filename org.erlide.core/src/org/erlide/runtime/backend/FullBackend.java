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

import java.util.Collection;

import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.IDisposable;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.console.IShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.internal.CodeManager;
import org.erlide.runtime.backend.internal.RuntimeLauncher;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class FullBackend extends Backend implements IDisposable {

	private final CodeManager fCodeManager;
	private IShellManager fShellManager;

	FullBackend(final RuntimeInfo info, final RuntimeLauncher launcher)
			throws BackendException {
		super(info, launcher);
		fCodeManager = new CodeManager(this);
		fShellManager = new BackendShellManager(this);
	}

	@Override
	public void dispose() {
		dispose(false);
	}

	@Override
	public void dispose(final boolean restart) {
		ErlLogger.debug("disposing backend " + getName());
		super.dispose(restart);
		if (fShellManager instanceof IDisposable) {
			((IDisposable) fShellManager).dispose();
		}
	}

	CodeManager getCodeManager() {
		return fCodeManager;
	}

	public IShellManager getShellManager() {
		return fShellManager;
	}

	@Override
	public void initializeRuntime() {
		super.initializeRuntime();
		fShellManager = new BackendShellManager(this);
	}

	public void removePath(final boolean usePathZ, final String path) {
		fCodeManager.removePath(usePathZ, path);
	}

	public void addPath(final boolean usePathZ, final String path) {
		fCodeManager.addPath(usePathZ, path);
	}

	@Override
	public void connectAndRegister(Collection<ICodeBundle> plugins) {
		super.connectAndRegister(plugins);
		if (plugins != null) {
			for (final ICodeBundle element : plugins) {
				getCodeManager().register(element);
			}
		}
		getCodeManager().registerBundles();
		checkCodePath();
	}
}
