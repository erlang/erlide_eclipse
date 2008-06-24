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
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basicui.util.PopupDialog;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.runtime.ErlangLaunchPlugin;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.debug.ErlangDebugTarget;

import erlang.ErlideDebug;

public class ErlangNodeLaunchConfigurationDelegate extends
		LaunchConfigurationDelegate {

	public void launch(final ILaunchConfiguration configuration,
			final String mode, final ILaunch launch,
			final IProgressMonitor monitor) throws CoreException {

		// final boolean separateNode = useSeparateNode(configuration);

		try {
			final String projectName = configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME, "");
			// ( debugmodel? bäst kolla upp nu igen)
			final String label = configuration.getAttribute(
					IProcess.ATTR_PROCESS_LABEL, projectName);
			final String nodeName = BackendManager.buildNodeName(label);

			final String nameAndCookie = "-name " + nodeName + " -setcookie "
					+ Cookie.retrieveCookie();

			final String mod = getStartModule(configuration);
			final String fn = getStartFunc(configuration);

			// launch of erlang project

			// build command string
			final StringBuilder cmd = new StringBuilder();
			cmd.append(configuration.getAttribute(
					IErlangLaunchConfigurationAttributes.ATTR_OTP_HOME, ""));
			if (cmd.length() > 0) {
				cmd.append(File.separator).append("bin").append(File.separator)
						.append("erl ");
			}
			cmd.append(nameAndCookie).append(" ");
			IProject project = null;
			if (projectName.length() > 0) {
				project = ResourcesPlugin.getWorkspace().getRoot().getProject(
						projectName);
				final ErlangProjectProperties prefs = new ErlangProjectProperties(
						project);
				final String projOutputDir = project.getLocation().append(
						prefs.getOutputDir()).toOSString();
				if (projOutputDir.length() > 0) {
					cmd.append((prefs.getUsePathZ() ? "-pz" : "-pa")).append(
							" ").append(projOutputDir).append(" ");
				}
			}
			// if (mod.length() > 0 && fn.length() > 0) {
			// cmd.append("-s ").append(mod).append(" ").append(fn)
			// .append(" ");
			// }
			ErlLogger.debug("RUN*> " + cmd.toString());
			// launch an erlang process
			final File workingDirectory = new File(".");
			Process vm = null;
			IProcess process = null;
			try {
				vm = Runtime.getRuntime().exec(cmd.toString(), null,
						workingDirectory);
				process = new ErtsProcess(launch, vm, label, null);
				launch.addProcess(process);
			} catch (final Exception e) {
			}

			if (vm == null) {
				PopupDialog
						.showBalloon(
								"Starting Erlang backend",
								"Could not start, please check your preferences!",
								3000);
			}
			// make a nice little BackEnd for it
			final IBackend backend = getBackend(label);
			// backend.setLabel(label);
			backend.setErts(process);
			if (mode.equals(ILaunchManager.DEBUG_MODE)) {
				// load the debugger code on this erlang node
				final List<ICodeBundle> l = new ArrayList<ICodeBundle>(1);
				l.add(ErlangLaunchPlugin.getDefault());
				backend.connectAndRegister(l);
				// add debug target
				final ErlangDebugTarget target = new ErlangDebugTarget(launch,
						backend);
				launch.addDebugTarget(target);
				if (project != null) {
					interpretAll(backend, project);
				}
			}
			if (mod.length() > 0 && fn.length() > 0) {
				backend.rpc(mod, fn, "");
			}
		} catch (final Exception e) {
			ErlLogger.debug("Could not launch Erlang:::");
			e.printStackTrace();
		}

	}

	private void interpretAll(final IBackend backend, final IProject project) {
		final List<String> beams = new ArrayList<String>();
		final List<String> erls = new ArrayList<String>();
		try {
			project.accept(new IResourceVisitor() {
				public boolean visit(final IResource resource)
						throws CoreException {
					final IPath fullPath = resource.getFullPath();
					if (fullPath != null) {
						final String ext = fullPath.getFileExtension();
						if (ext != null) {
							final String baseName = fullPath
									.removeFileExtension().lastSegment();
							if (ext.equals("beam")) {
								beams.add(baseName);
							} else if (ext.equals("erl")) {
								erls.add(baseName);
							}
						}
					}
					return true;
				}
			}, IResource.DEPTH_INFINITE, 0);
			for (final String erl : erls) {
				if (beams.contains(erl)) {
					ErlideDebug.interpret(backend, erl);
				}
			}

		} catch (final CoreException e) {
			// TODO Auto-generated catch block
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

	public IBackend getBackend(final String name) {
		return BackendManager.getDefault().getNamedBackend(name, false);
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
