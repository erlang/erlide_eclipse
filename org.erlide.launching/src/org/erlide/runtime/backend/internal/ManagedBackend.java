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

import org.eclipse.core.runtime.Assert;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basiccore.ErtsPreferences;
import org.erlide.basicui.ErlideBasicUIPlugin;
import org.erlide.basicui.prefs.IPrefConstants;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.Cookie;
import org.erlide.runtime.backend.ErtsProcess;
import org.erlide.runtime.backend.console.BackendShellManager;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class ManagedBackend extends AbstractBackend {

	private ErtsProcess fErts;
	static private boolean dontUseLaunchConfigurationForInternalErts = true;

	// private ILaunch startErts() {
	// if (getLabel() == null) {
	// return null;
	// }
	//
	// final String cmd = getCmdLine();
	//
	// try {
	// final ILaunchManager manager = DebugPlugin.getDefault()
	// .getLaunchManager();
	// final ILaunchConfigurationType type = manager
	// .getLaunchConfigurationType(ErtsProcess.ERLIDE_CONFIGURATION_TYPE);
	// ILaunchConfigurationWorkingCopy wc;
	// wc = type.newInstance(null, getLabel());
	// wc.setAttribute(IProcess.ATTR_PROCESS_LABEL, getLabel());
	// wc.setAttribute(IProcess.ATTR_PROCESS_TYPE, "erlang vm");
	// wc.setAttribute(IProcess.ATTR_CMDLINE, cmd);
	// wc.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID,
	// ErtsProcessFactory.ID);
	// wc.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, true);
	//
	// final ILaunch ll = wc.launch(ILaunchManager.RUN_MODE,
	// new NullProgressMonitor());
	// fErts = null;
	// if (ll.getProcesses().length == 1) {
	// fErts = (ErtsProcess) ll.getProcesses()[0];
	// }
	//
	// fShellManager = new BackendShellManager(this);
	// return ll;
	//
	// } catch (final Exception e) {
	// e.printStackTrace();
	// return null;
	// }
	// }

	static public String getCmdLine() {
		final ErtsPreferences ertsPrefs = ErlideBasicUIPlugin.getDefault()
				.getPreferences();
		final String otpHome = ErlideBasicUIPlugin.getDefault()
				.getPluginPreferences().getString(IPrefConstants.ERTS_OTP_HOME);
		final String cmd = ertsPrefs.buildCommandLine(otpHome);
		return cmd;
	}

	@Override
	public void connect(final String cookie) {
		if (fErts == null && !dontUseLaunchConfigurationForInternalErts) {
			return;
		}

		fLabel = BackendManager.buildNodeLabel(getLabel());
		doConnect(getLabel(), cookie);
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
		if (dontUseLaunchConfigurationForInternalErts) {
			if (streamsProxy != null) {
				streamsProxy.write(string);
			}
		} else {
			if (fErts == null) {
				return;
			}
			fErts.writeToErlang(string);
		}
	}

	public ErtsPreferences getNodePrefs() {
		if (fErts == null) {
			return null;
		}

		return fErts.getConfiguration();
	}

	@Override
	public void addStdListener(final IStreamListener dsp) {
		fErts.addStdListener(dsp);
	}

	@Override
	public void initializeErts() {
		if (dontUseLaunchConfigurationForInternalErts) {
			startErtsWOLC();
		} else {
			Assert.isTrue(dontUseLaunchConfigurationForInternalErts);
			// startErts();
		}
	}

	IStreamsProxy streamsProxy;

	private void startErtsWOLC() {
		if (getLabel() == null) {
			return;
		}

		String cmd = getCmdLine();

		final String label = getLabel();
		final String nodeName = BackendManager.buildNodeName(label);
		final String nameAndCookie = "-name " + nodeName + " -setcookie "
				+ Cookie.retrieveCookie();
		cmd += nameAndCookie;
		ErlLogger.debug("RUN internal node WOLC> " + cmd);
		final File workingDirectory = new File(".");
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
	public void setErts(final IProcess process) {
		if (process instanceof ErtsProcess) {
			fErts = (ErtsProcess) process;
		}
		fShellManager = new BackendShellManager(this);
	}
}
