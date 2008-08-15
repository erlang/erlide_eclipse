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

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.basiccore.ErlLogger;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.backend.ErtsProcessFactory;
import org.erlide.runtime.backend.console.BackendShellManager;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class ManagedBackend extends AbstractBackend {

	public ManagedBackend(RuntimeInfo info) {
		super(info);
	}

	private ErtsProcess fRuntime;
	static private boolean useLaunchConfigurationForInternalRuntime = false;

	@Override
	public void connect() {
		if (fRuntime == null && useLaunchConfigurationForInternalRuntime) {
			return;
		}

		fLabel = BackendManager.buildNodeLabel(getName());
		doConnect(getName());
	}

	/**
	 * Method dispose
	 */
	@Override
	public void dispose() {
		// BackendManager.getDefault().remove();
		if (fRuntime != null) {
			try {
				fRuntime.terminate();
			} catch (final DebugException e) {
			}
		}
		super.dispose();
	}

	// /**
	// * @param string
	// * @throws IOException
	// */
	// @Override
	// public void sendToShell(final String string) {
	// if (fErts == null) {
	// return;
	// }
	//
	// fErts.sendToShell(string);
	// }

	/**
	 * @param string
	 * @throws IOException
	 */
	@Override
	public void sendToDefaultShell(final String string) throws IOException {
		if (!useLaunchConfigurationForInternalRuntime) {
			if (streamsProxy != null) {
				streamsProxy.write(string);
			}
		} else {
			if (fRuntime == null) {
				return;
			}
			fRuntime.writeToErlang(string);
		}
	}

	@Override
	public void addStdListener(final IStreamListener dsp) {
		if (fRuntime == null) {
			return;
		}
		fRuntime.addStdListener(dsp);
	}

	@Override
	public void initializeRuntime() {
		if (!useLaunchConfigurationForInternalRuntime) {
			startRuntimeWOLC();
		} else {
			startRuntime();
		}
	}

	IStreamsProxy streamsProxy;

	private void startRuntimeWOLC() {
		if (getInfo() == null) {
			return;
		}

		String cmd = getInfo().getCmdLine();

		ErlLogger.debug("RUN internal node WOLC> " + cmd);
		final File workingDirectory = new File(getInfo().getWorkingDir());
		Process vm = null;
		try {
			vm = Runtime.getRuntime().exec(cmd, null, workingDirectory);
		} catch (final IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// final IProcess process = DebugPlugin.newProcess(launch, vm,
		// nodeName);
		// TODO kolla ErtsProcess' ärvda constructor!

		streamsProxy = new StreamsProxy(vm, null);
		fShellManager = new BackendShellManager(this);
	}

	@Override
	public void setRuntime(final IProcess process) {
		if (process instanceof ErtsProcess) {
			fRuntime = (ErtsProcess) process;
			fShellManager = new BackendShellManager(this);
		}
	}

	private ILaunch startRuntime() {
		if (getInfo() == null) {
			return null;
		}

		final String cmd = getInfo().getCmdLine();

		try {
			final ILaunchManager manager = DebugPlugin.getDefault()
					.getLaunchManager();
			final ILaunchConfigurationType type = manager
					.getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE);
			ILaunchConfigurationWorkingCopy wc;
			wc = type.newInstance(null, getInfo().getName());
			wc.setAttribute(IProcess.ATTR_PROCESS_LABEL, getInfo().getName());
			wc.setAttribute(IProcess.ATTR_PROCESS_TYPE, "erlang vm");
			wc.setAttribute(IProcess.ATTR_CMDLINE, cmd);
			wc.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID,
					ErtsProcessFactory.ID);
			wc.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, true);

			final ILaunch ll = wc.launch(ILaunchManager.RUN_MODE,
					new NullProgressMonitor());
			fRuntime = null;
			if (ll.getProcesses().length == 1) {
				fRuntime = (ErtsProcess) ll.getProcesses()[0];
			}

			fShellManager = new BackendShellManager(this);
			return ll;

		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}
	}

}
