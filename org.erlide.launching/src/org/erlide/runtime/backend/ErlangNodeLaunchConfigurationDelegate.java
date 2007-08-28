/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basicui.ErlideBasicUIPlugin;

public class ErlangNodeLaunchConfigurationDelegate extends
		LaunchConfigurationDelegate {

	public void launch(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		// final boolean separateNode = useSeparateNode(configuration);

		// TODO define all launch config attributes

		try {
			String cmd = getCmdLine(configuration);

			String label = configuration.getAttribute(
					IProcess.ATTR_PROCESS_LABEL, "noname@localhost");
			label = BackendManager.buildNodeName(label);

			cmd += " -noshell -name " + label + " -setcookie " +
					Cookie.retrieveCookie();

			ErlLogger.debug("RUN*> " + cmd);

			final File workingDirectory = new File(".");
			Process vm = null;
			int tries = 3;
			while (vm == null && tries > 1) {
				try {
					vm = Runtime.getRuntime().exec(cmd, null, workingDirectory);
					final Map<String, String> defaultAttributes = new HashMap<String, String>();
					defaultAttributes.put(IProcess.ATTR_PROCESS_TYPE, "erlang");
					defaultAttributes.put(IProcess.ATTR_PROCESS_LABEL, label);
					defaultAttributes.put(IProcess.ATTR_CMDLINE, cmd);

					final IProcess process = DebugPlugin.newProcess(launch, vm,
							label, defaultAttributes);

					launch.addProcess(process);
				} catch (final Exception e) {
					tries--;
					ErlideBasicUIPlugin.showErtsPreferencesDialog(tries - 1);
				}
			}
			if (vm == null) {
				ErlideBasicUIPlugin.closeWorkbench();
			}

			// if (mode.equals(ILaunchManager.DEBUG_MODE)) {
			// final IBackend b = getBackend(configuration);
			// ErlLogger.debug("DEBUG*> " + mod + ":" + func + "();...");
			// final IDebugTarget target = new ErlangDebugTarget(launch, b,
			// mod, func);
			// launch.addDebugTarget(target);
			//
			// // TODO initialize debugger
			// }

		} catch (final Exception e) {
			ErlLogger.debug("Could not launch Erlang:::");
			e.printStackTrace();
		}

	}

	public String getStartFunc(ILaunchConfiguration configuration) {
		try {
			return configuration
					.getAttribute(
							IErlangLaunchConfigurationAttributes.ATTR_ENODE_FUNCTION,
							IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION);
		} catch (final CoreException e) {
			return IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_FUNCTION;
		}
	}

	public String getCmdLine(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(IProcess.ATTR_CMDLINE, "");
		} catch (final CoreException e) {
			return "";
		}
	}

	public String getStartModule(ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_ENODE_MODULE,
					IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE);
		} catch (final CoreException e) {
			return IErlangLaunchConfigurationAttributes.DEFAULT_ENODE_MODULE;
		}
	}

	public IBackend getBackend(ILaunchConfiguration configuration) {
		// TODO use project backend
		return BackendManager.getDefault().getIdeBackend();
	}

	/* NOT USED */
	/*
	 * private boolean useSeparateNode(ILaunchConfiguration configuration) { //
	 * false = use project's backend // true = start new backend return false; }
	 */

}
