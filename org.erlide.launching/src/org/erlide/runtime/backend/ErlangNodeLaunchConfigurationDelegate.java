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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basicui.util.PopupDialog;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.internal.ManagedBackend;
import org.erlide.runtime.debug.ErlangDebugTarget;

public class ErlangNodeLaunchConfigurationDelegate extends
		LaunchConfigurationDelegate {

	public void launch(final ILaunchConfiguration configuration,
			final String mode, final ILaunch launch,
			final IProgressMonitor monitor) throws CoreException {
		// final boolean separateNode = useSeparateNode(configuration);

		// TODO define all launch config attributes

		try {
			// ( debugmodel? bŠst kolla upp nu igen)
			String label = configuration.getAttribute(
					IProcess.ATTR_PROCESS_LABEL, "noname");
			label = BackendManager.buildNodeName(label);

			final String nameAndCookie = "-name " + label + " -setcookie "
					+ Cookie.retrieveCookie();

			// If ATTR_CMDLINE is set, it's an launch for internal ErlIde
			String cmd;
			final String mod = getStartModule(configuration);
			final String fn = getStartFunc(configuration);

			if (configuration.getAttribute(IProcess.ATTR_CMDLINE, "").length() > 0) {
				// ErlIDE internal node
				cmd = configuration.getAttribute(IProcess.ATTR_CMDLINE, "")
						+ " -noshell " + nameAndCookie + " ";
			} else {
				// launch of erlang project
				cmd = configuration.getAttribute(
						IErlangLaunchConfigurationAttributes.ATTR_OTP_HOME, "");
				if (cmd.length() > 0) {
					cmd += File.separator + "bin" + File.separator + "erl ";
				}
				cmd += nameAndCookie + " ";
				final String projectName = configuration.getAttribute(
						IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME,
						"");
				if (projectName.length() > 0) {
					final IProject project = ResourcesPlugin.getWorkspace()
							.getRoot().getProject(projectName);
					final ErlangProjectProperties prefs = new ErlangProjectProperties(
							project);
					final String projOutputDir = project.getLocation().append(
							prefs.getOutputDir()).toOSString();
					if (projOutputDir.length() > 0) {
						cmd += (prefs.getUsePathZ() ? "-pz" : "-pa") + " "
								+ projOutputDir + " ";
					}
				}
				if (mod.length() > 0 && fn.length() > 0) {
					cmd += "-s " + mod + " " + fn + " ";
				}
			}
			if (mode.equals(ILaunchManager.DEBUG_MODE)) {
				final IDebugTarget target = new ErlangDebugTarget(launch,
						getBackend(configuration), mod, fn);
				launch.addDebugTarget(target);
			}
			ErlLogger.debug("RUN*> " + cmd);
			final File workingDirectory = new File(".");
			Process vm = null;

			try {
				vm = Runtime.getRuntime().exec(cmd, null, workingDirectory);

				final IProcess process = DebugPlugin.newProcess(launch, vm,
						label);

				launch.addProcess(process);
			} catch (final Exception e) {
			}
			cmd = ManagedBackend.getCmdLine() + nameAndCookie;

			if (vm == null) {
				PopupDialog
						.showBalloon(
								"Starting Erlang backend",
								"Could not start, please check your preferences!",
								3000);
			}

		} catch (final Exception e) {
			ErlLogger.debug("Could not launch Erlang:::");
			e.printStackTrace();
		}

	}

	public String getStartFunc(final ILaunchConfiguration configuration) {
		try {
			return configuration
					.getAttribute(
							IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
							IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION);
		} catch (final CoreException e) {
			return IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION;
		}
	}

	public String getCmdLine(final ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(IProcess.ATTR_CMDLINE, "");
		} catch (final CoreException e) {
			return "";
		}
	}

	public String getStartModule(final ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
					IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE);
		} catch (final CoreException e) {
			return IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE;
		}
	}

	public IBackend getBackend(final ILaunchConfiguration configuration) {
		// TODO use project backend
		return BackendManager.getDefault().getIdeBackend();
	}

	protected String getAdditionalArgs(final ILaunchConfiguration configuration) {
		return "";
	}

	/* NOT USED */
	/*
	 * private boolean useSeparateNode(ILaunchConfiguration configuration) { //
	 * false = use project's backend // true = start new backend return false; }
	 */

}
