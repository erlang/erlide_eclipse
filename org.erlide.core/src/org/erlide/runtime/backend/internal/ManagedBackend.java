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
package org.erlide.runtime.backend.internal;

import java.io.File;
import java.io.IOException;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class ManagedBackend extends AbstractBackend {

	public ManagedBackend(RuntimeInfo info) throws BackendException {
		super(info);
	}

	Process fRuntime;

	@Override
	public void connect() {
		doConnect(getName());
	}

	public void stop() {
		if (fRuntime != null) {
			fRuntime.destroy();
		}
	}

	/**
	 * Method dispose
	 */
	@Override
	public void dispose() {
		stop();
		super.dispose();
	}

	@Override
	public void initializeRuntime(ILaunch launch) {
		startRuntime(launch);
	}

	IStreamsProxy streamsProxy;
	private ErtsProcess erts;

	private void startRuntime(ILaunch launch) {
		if (getInfo() == null) {
			return;
		}

		String cmd = getInfo().getCmdLine();

		ErlLogger.debug("START node :> " + cmd);
		final File workingDirectory = new File(getInfo().getWorkingDir());
		try {
			fRuntime = Runtime.getRuntime().exec(cmd, null, workingDirectory);
			Runnable watcher = new Runnable() {
				@SuppressWarnings("boxing")
				public void run() {
					try {
						int v = fRuntime.waitFor();
						ErlLogger
								.error(
										"Backend runtime %s terminated with exit code %d.",
										getInfo().getNodeName(), v);
					} catch (InterruptedException e) {
						ErlLogger.warn("ide backend watcher was interrupted");
					}
				}
			};
			new Thread(watcher).start();

			if (launch != null) {
				erts = new ErtsProcess(launch, fRuntime, getInfo()
						.getNodeName(), null);
				launch.addProcess(erts);
			}
		} catch (final IOException e) {
			ErlLogger.error(e);
		}

		// streamsProxy = new StreamsProxy(fRuntime, null);
		fShellManager = new BackendShellManager(this);
	}

}
