/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *     Jakob Cederlund
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.File;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basicui.util.PopupDialog;
import org.erlide.runtime.backend.internal.ManagedBackend;

public class ErlideNodeLaunchConfigurationDelegate extends
		LaunchConfigurationDelegate {

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		// final boolean separateNode = useSeparateNode(configuration);

		// TODO define all launch config attributes

		try {
			// TODO split erts-launch for ErlIde needs and Erlang-launch into
			// two separate LaunchConfigurations?

			// We have two kind of launches to handle here
			String label = configuration.getAttribute(
					IProcess.ATTR_PROCESS_LABEL, "noname");
			label = BackendManager.buildNodeName(label);

			final String nameAndCookie = "-name " + label + " -setcookie "
					+ Cookie.retrieveCookie();

			// If ATTR_CMDLINE is set, it's an launch for internal ErlIde
			String cmd;
			// ErlIDE internal node
			cmd = ManagedBackend.getCmdLine() + nameAndCookie;
			cmd = configuration.getAttribute(IProcess.ATTR_CMDLINE, "")
					+ " -noshell " + nameAndCookie + " ";
			ErlLogger.debug("RUN> " + cmd);
			final File workingDirectory = new File(".");
			Process vm = null;

			try {
				vm = Runtime.getRuntime().exec(cmd, null, workingDirectory);

				final IProcess process = DebugPlugin.newProcess(launch, vm,
						label);

				launch.addProcess(process);
			} catch (final Exception e) {
			}

			if (vm == null) {
				PopupDialog
						.showDialog(
								"Starting Erlang backend",
								"Could not start, please check your preferences!",
								4000);

			}

		} catch (final Exception e) {
			ErlLogger.debug("Could not launch Erlang:::");
			e.printStackTrace();
		}

	}

	public String getCmdLine(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(IProcess.ATTR_CMDLINE, "");
		} catch (final CoreException e) {
			return "";
		}
	}

	public IBackend getBackend(ILaunchConfiguration configuration) {
		// TODO use project backend
		return BackendManager.getDefault().getIdeBackend();
	}

	protected String getAdditionalArgs(ILaunchConfiguration configuration) {
		return "";
	}

	/* NOT USED */
	/*
	 * private boolean useSeparateNode(ILaunchConfiguration configuration) { //
	 * false = use project's backend // true = start new backend return false; }
	 */

}
