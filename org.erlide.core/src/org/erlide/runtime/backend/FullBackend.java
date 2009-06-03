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

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.backend.IDisposable;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeLauncher;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.events.EventDaemon;
import org.erlide.runtime.backend.internal.CodeManager;
import org.erlide.runtime.backend.internal.LogEventHandler;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class FullBackend extends Backend implements IDisposable {

	private final CodeManager fCodeManager;
	private IShellManager fShellManager;
	private EventDaemon eventDaemon;

	public FullBackend(final RuntimeInfo info, final RuntimeLauncher launcher)
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
		if (eventDaemon != null) {
			eventDaemon.stop();
		}
	}

	public CodeManager getCodeManager() {
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

	public void connectAndRegister(Collection<ICodeBundle> plugins) {
		connect();
		if (plugins != null) {
			for (final ICodeBundle element : plugins) {
				getCodeManager().register(element);
			}
		}
		getCodeManager().registerBundles();
		checkCodePath();
	}

	public EventDaemon getEventDaemon() {
		return eventDaemon;
	}

	@Override
	public void initErlang() {
		super.initErlang();

		eventDaemon = new EventDaemon(this);
		eventDaemon.start();
		eventDaemon.addListener(new LogEventHandler());
	}
}
