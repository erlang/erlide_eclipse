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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IBreakpointManager;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.IBreakpoint;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.util.ErlideUtil;
import org.erlide.jinterface.JInterfaceFactory;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.BackendManager.BackendOptions;
import org.erlide.runtime.debug.ErlDebugConstants;
import org.erlide.runtime.debug.ErlangDebugTarget;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideDebug;

public class ErlangLaunchConfigurationDelegate extends
		LaunchConfigurationDelegate {

	public void launch(final ILaunchConfiguration config, final String mode,
			final ILaunch launch, final IProgressMonitor monitor)
			throws CoreException {
		doLaunch(config, mode, launch, false);
	}

	@SuppressWarnings("unchecked")
	private void doLaunch(final ILaunchConfiguration config, final String mode,
			final ILaunch launch, boolean internal) {
		try {
			final String prjs = config.getAttribute(
					ErlLaunchAttributes.PROJECTS, "").trim();
			final String[] projectNames = prjs.length() == 0 ? new String[] {}
					: prjs.split(";");
			final String module = config.getAttribute(
					ErlLaunchAttributes.MODULE, "").trim();
			final String function = config.getAttribute(
					ErlLaunchAttributes.FUNCTION, "").trim();
			final String args = config.getAttribute(
					ErlLaunchAttributes.ARGUMENTS, "").trim();
			final String runtime = config.getAttribute(
					ErlLaunchAttributes.RUNTIME_NAME, "").trim();
			final String nodeName = config.getAttribute(
					ErlLaunchAttributes.NODE_NAME, "").trim();
			String cookie = config.getAttribute(ErlLaunchAttributes.COOKIE, "")
					.trim();
			final boolean startMe = config.getAttribute(
					ErlLaunchAttributes.START_ME, internal);
			final int debugFlags = config.getAttribute(
					ErlLaunchAttributes.DEBUG_FLAGS,
					ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
			List<String> interpretedModules = config.getAttribute(
					ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
					new ArrayList<String>());

			final Set<IProject> projects = new HashSet<IProject>();
			for (final String s : projectNames) {
				final IProject project = ResourcesPlugin.getWorkspace()
						.getRoot().getProject(s);
				if (project == null) {
					ErlLogger.error("Launch: project not found: '%s'!", s);
					return;
				}
				projects.add(project);
			}
			interpretedModules = addBreakpointProjectsAndModules(projects,
					interpretedModules);

			final RuntimeInfo rt = RuntimeInfo.copy(ErlangCore
					.getRuntimeInfoManager().getRuntime(runtime), false);
			rt.setNodeName(nodeName);
			if (internal) {
				cookie = "erlide";
			}
			rt.setCookie(cookie);

			final EnumSet<BackendOptions> options = EnumSet
					.noneOf(BackendOptions.class);
			if (mode.equals(ILaunchManager.DEBUG_MODE)) {
				options.add(BackendOptions.DEBUG);
			}
			if (startMe) {
				options.add(BackendOptions.AUTOSTART);
			}

			System.out.println("Debug:: about to start a backend in " + mode
					+ " mode, with attributes::");
			System.out.println("  projects: " + Arrays.toString(projectNames));
			System.out.println("  module: " + module);
			System.out.println("  function: " + function);
			System.out.println("  args: " + args);

			System.out.println("  runtime: " + runtime);
			System.out.println("  node name: " + nodeName);
			System.out.println("  cookie: " + cookie);
			System.out.println("  debugFlags: " + debugFlags);
			System.out.println("  interpretedModules: " + interpretedModules);
			if (startMe) {
				System.out.println("  * start it if not running");
			}
			System.out.println("---------------");

			final Backend b = ErlangCore.getBackendManager().create(rt,
					options, launch);
			if (b == null) {
				ErlLogger.error("Launch: got null backend!");
				return;
			}
			final Backend backend = b;

			// launch.addProcess(null);
			backend.registerProjects(projects);
			if (mode.equals(ILaunchManager.DEBUG_MODE)) {
				// add debug target
				final ErlangDebugTarget target = new ErlangDebugTarget(launch,
						backend, projects, debugFlags);
				// target.getWaiter().doWait();
				launch.addDebugTarget(target);
				// interpret everything we can
				final boolean distributed = (debugFlags & ErlDebugConstants.DISTRIBUTED_DEBUG) != 0;
				if (distributed) {
					distributeDebuggerCode(backend);
				}
				for (final String pm : interpretedModules) {
					final String[] pms = pm.split(":");
					interpret(backend, pms[0], pms[1], distributed, true);
				}
				// send started to target
				target.sendStarted();

				DebugPlugin.getDefault().addDebugEventListener(
						new IDebugEventSetListener() {

							public void handleDebugEvents(
									final DebugEvent[] events) {

								try {
									if (module.length() > 0
											&& function.length() > 0) {
										if (args.length() > 0) {
											// TODO issue #84
											backend.rpc(module, function, "s",
													args);
										} else {
											backend.rpc(module, function, "");
										}
									}
								} catch (final Exception e) {
									ErlLogger
											.debug(
													"Could not run initial call %s:%s(\"%s\")",
													module, function, args);
									ErlLogger.warn(e);
								}
								DebugPlugin.getDefault()
										.removeDebugEventListener(this);
							}
						});
			} else {
				try {
					if (module.length() > 0 && function.length() > 0) {
						if (args.length() > 0) {
							// TODO issue #84
							backend.rpc(module, function, "s", args);
						} else {
							backend.rpc(module, function, "");
						}
					}
				} catch (final Exception e) {
					ErlLogger.debug("Could not run initial call %s:%s(\"%s\")",
							module, function, args);
					ErlLogger.warn(e);
				}
			}
		} catch (final Exception e) {
			ErlLogger.debug("Could not launch Erlang:::");
			ErlLogger.warn(e);
		}
	}

	public static List<String> addBreakpointProjectsAndModules(
			final Collection<IProject> projects,
			final List<String> interpretedModules) {
		final IBreakpointManager bpm = DebugPlugin.getDefault()
				.getBreakpointManager();
		final List<String> result = new ArrayList<String>(interpretedModules);
		for (final IBreakpoint bp : bpm
				.getBreakpoints(ErlDebugConstants.ID_ERLANG_DEBUG_MODEL)) {
			final IMarker m = bp.getMarker();
			final IResource r = m.getResource();
			final String name = r.getName();
			if (ErlideUtil.hasERLExtension(name)) {
				final IProject p = r.getProject();
				if (projects.contains(p)) {
					final String s = p.getName() + ":" + name;
					if (!result.contains(s)) {
						result.add(s);
					}
				}
			}
		}
		return result;
	}

	private void distributeDebuggerCode(final Backend backend) {
		final String debuggerModules[] = { "erlide_dbg_debugged",
				"erlide_dbg_icmd", "erlide_dbg_idb", "erlide_dbg_ieval",
				"erlide_dbg_iload", "erlide_dbg_iserver", "erlide_int", "int" };
		final List<OtpErlangTuple> modules = new ArrayList<OtpErlangTuple>(
				debuggerModules.length);
		for (final String module : debuggerModules) {
			final OtpErlangBinary b = getBeam(module, backend);
			if (b != null) {
				final OtpErlangString filename = new OtpErlangString(module
						+ ".erl");
				final OtpErlangTuple t = JInterfaceFactory.mkTuple(
						new OtpErlangAtom(module), filename, b);
				modules.add(t);
			}
		}
		ErlideDebug.distributeDebuggerCode(backend, modules);
	}

	/**
	 * Get a named beam-file as a binary from the core plug-in bundle
	 * 
	 * @param module
	 *            module name, without extension
	 * @param backend
	 *            the execution backend
	 * @return
	 */
	private OtpErlangBinary getBeam(final String module, final Backend backend) {
		final Bundle b = ErlangPlugin.getDefault().getBundle();
		final String beamname = module + ".beam";
		final IExtensionRegistry reg = RegistryFactory.getRegistry();
		final IConfigurationElement[] els = reg.getConfigurationElementsFor(
				ErlangPlugin.PLUGIN_ID, "codepath");
		// TODO: this code assumes that the debugged target and the
		// erlide-plugin uses the same Erlang version, how can we escape this?
		final String ver = backend.getCurrentVersion();
		for (final IConfigurationElement el : els) {
			final IContributor c = el.getContributor();
			if (c.getName().equals(b.getSymbolicName())) {
				final String dir_path = el.getAttribute("path");
				Enumeration<?> e = b.getEntryPaths(dir_path + "/" + ver);
				if (e == null || !e.hasMoreElements()) {
					e = b.getEntryPaths(dir_path);
				}
				if (e == null) {
					ErlLogger.debug("* !!! error loading plugin "
							+ b.getSymbolicName());
					return null;
				}
				while (e.hasMoreElements()) {
					final String s = (String) e.nextElement();
					final Path path = new Path(s);
					if (path.lastSegment().equals(beamname)) {
						if (path.getFileExtension() != null
								&& "beam".compareTo(path.getFileExtension()) == 0) {
							final String m = path.removeFileExtension()
									.lastSegment();
							try {
								return ErlideUtil.getBeamBinary(m, b
										.getEntry(s));
							} catch (final Exception ex) {
								ErlLogger.warn(ex);
							}
						}
					}
				}
			}
		}
		return null;
	}

	public static void interpret(final Backend backend, final String project,
			final String module, final boolean distributed,
			final boolean interpret) {
		final IErlProject eprj = ErlangCore.getModel()
				.getErlangProject(project);
		final IProject iprj = eprj.getProject();
		try {
			final IFolder r = iprj.getFolder(eprj.getOutputLocation());
			final String beam = ErlideUtil.withoutExtension(module) + ".beam";
			final IFile f = r.getFile(beam);
			if (f.exists()) {
				final String de = interpret ? "" : "de";
				ErlLogger.debug(de + "interpret " + beam);
				ErlideDebug.interpret(backend, f.getLocation().toString(),
						distributed, interpret);
			} else {
				ErlLogger.debug("IGNORED MISSING interpret " + project + ":"
						+ module);
			}

		} catch (final ErlModelException e) {
			ErlLogger.warn(e);
		}

	}

	public String getCmdLine(final ILaunchConfiguration configuration) {
		try {
			return configuration.getAttribute(IProcess.ATTR_CMDLINE, "");
		} catch (final CoreException e) {
			return "";
		}
	}

	protected String getAdditionalArgs(final ILaunchConfiguration configuration) {
		return "";
	}

	public void launchInternal(ILaunchConfiguration configuration, String mode,
			ILaunch launch, IProgressMonitor monitor) {
		doLaunch(configuration, mode, launch, true);
	}

}
