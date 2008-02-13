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
import org.erlide.basiccore.ErtsPreferences;
import org.erlide.basicui.ErlideBasicUIPlugin;
import org.erlide.basicui.prefs.IPrefConstants;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.backend.ErtsProcessFactory;
import org.erlide.runtime.backend.console.BackendShellManager;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class ManagedBackend extends AbstractBackend {

	private ErtsProcess fErts;

	private ILaunch startErts() {
		if (getLabel() == null) {
			return null;
		}

		final String cmd = getCmdLine();

		try {
			final ILaunchManager manager = DebugPlugin.getDefault()
					.getLaunchManager();
			final ILaunchConfigurationType type = manager
					.getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE);
			ILaunchConfigurationWorkingCopy wc;
			wc = type.newInstance(null, getLabel());
			wc.setAttribute(IProcess.ATTR_PROCESS_LABEL, getLabel());
			wc.setAttribute(IProcess.ATTR_PROCESS_TYPE, "erlang vm");
			wc.setAttribute(IProcess.ATTR_CMDLINE, cmd);
			wc.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID,
					ErtsProcessFactory.ID);
			wc.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, true);

			final ILaunch ll = wc.launch(ILaunchManager.DEBUG_MODE,
					new NullProgressMonitor());
			fErts = null;
			if (ll.getProcesses().length == 1) {
				fErts = (ErtsProcess) ll.getProcesses()[0];
			}

			fShellManager = new BackendShellManager(this);
			return ll;

		} catch (final Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	static public String getCmdLine() {
		final ErtsPreferences ertsPrefs = ErlideBasicUIPlugin.getDefault()
				.getPreferences();
		String otpHome = ErlideBasicUIPlugin.getDefault()
				.getPluginPreferences().getString(IPrefConstants.ERTS_OTP_HOME);
		final String cmd = ertsPrefs.buildCommandLine(otpHome);
		return cmd;
	}

	@Override
	public void connect() {
		if (fErts == null) {
			return;
		}

		fLabel = BackendManager.buildNodeLabel(getLabel());
		doConnect(getLabel());
	}

	/**
	 * Method dispose
	 */
	@Override
	public void dispose() {
		super.dispose();

		// BackendManager.getDefault().remove();

		if (fErts != null) {
			try {
				fErts.terminate();
			} catch (final DebugException e) {
			}
		}
	}

	/**
	 * @param string
	 * @throws IOException
	 */
	@Override
	public void sendToShell(String string) {
		if (fErts == null) {
			return;
		}

		fErts.sendToShell(string);
	}

	/**
	 * @param string
	 * @throws IOException
	 */
	@Override
	public void sendToDefaultShell(String string) throws IOException {
		if (fErts == null) {
			return;
		}

		fErts.writeToErlang(string);
	}

	public ErtsPreferences getNodePrefs() {
		if (fErts == null) {
			return null;
		}

		return fErts.getConfiguration();
	}

	@Override
	public void addStdListener(IStreamListener dsp) {
		fErts.addStdListener(dsp);
	}

	@Override
	public ILaunch initialize() {
		return startErts();
	}

}
