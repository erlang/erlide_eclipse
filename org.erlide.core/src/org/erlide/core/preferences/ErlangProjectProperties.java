/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.core.preferences;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.RuntimeInfo;
import org.erlide.jinterface.backend.RuntimeVersion;
import org.erlide.jinterface.backend.util.PreferencesUtils;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.FullBackend;
import org.osgi.service.prefs.BackingStoreException;

public class ErlangProjectProperties implements IPreferenceChangeListener {

	private IProject project;

	private String sourceDirs = ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS;
	private String usePathZ = ProjectPreferencesConstants.DEFAULT_USE_PATHZ;
	private String outputDir = ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR;
	private String includeDirs = ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS;
	private String externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
	private String externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
	private RuntimeVersion runtimeVersion = new RuntimeVersion(
			ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
	private String runtimeName = null;

	public static final String CODEPATH_FILENAME = ".codepath"; //$NON-NLS-1$

	public ErlangProjectProperties() {
	}

	public ErlangProjectProperties(final IProject prj) {
		super();
		project = prj;
		final IEclipsePreferences root = new ProjectScope(project)
				.getNode(ErlangPlugin.PLUGIN_ID);
		root.addPreferenceChangeListener(this);
		// TODO load() should not be in constructor!
		load(root);
	}

	public void dispose() {
		new ProjectScope(project).getNode(ErlangPlugin.PLUGIN_ID)
				.removePreferenceChangeListener(this);
	}

	public ErlangProjectProperties load(final IEclipsePreferences node) {
		if (project == null) {
			return this;
		}

		if ("true".equals(System.getProperty("erlide.newprops"))) {
			final NewErlangProjectProperties npp = new NewErlangProjectProperties();
			try {
				npp.load((IEclipsePreferences) node.node("test"));
				npp.store((IEclipsePreferences) node.node("new_test"));
			} catch (final BackingStoreException e) {
				e.printStackTrace();
			}
		}

		final IFile cp = project.getFile(CODEPATH_FILENAME);
		if (cp.exists()) {
			final String msg = "Found old configuration file %s for project %s, please remove it.";
			ErlLogger.warn(msg, CODEPATH_FILENAME, project.getName());
		}

		sourceDirs = node.get(ProjectPreferencesConstants.SOURCE_DIRS,
				ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
		includeDirs = node.get(ProjectPreferencesConstants.INCLUDE_DIRS,
				ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
		outputDir = node.get(ProjectPreferencesConstants.OUTPUT_DIR,
				ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
		usePathZ = node.get(ProjectPreferencesConstants.USE_PATHZ,
				ProjectPreferencesConstants.DEFAULT_USE_PATHZ);
		runtimeVersion = new RuntimeVersion(node.get(
				ProjectPreferencesConstants.RUNTIME_VERSION, null));
		runtimeName = node.get(ProjectPreferencesConstants.RUNTIME_NAME, null);
		if (!runtimeVersion.isDefined()) {
			if (runtimeName == null) {
				runtimeVersion = new RuntimeVersion(
						ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
			} else {
				final RuntimeInfo ri = ErlangCore.getRuntimeInfoManager()
						.getRuntime(runtimeName);
				if (ri != null) {
					runtimeVersion = new RuntimeVersion(ri.getVersion());
				} else {
					runtimeVersion = new RuntimeVersion(
							ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
				}
			}
		}
		externalModulesFile = node.get(
				ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
				ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES);
		externalIncludesFile = node.get(
				ProjectPreferencesConstants.EXTERNAL_INCLUDES,
				ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES);
		return this;
	}

	public void store(final IEclipsePreferences node) {
		if (project == null) {
			return;
		}

		if ("true".equals(System.getProperty("erlide.newprops"))) {
			final NewErlangProjectProperties npp = new NewErlangProjectProperties(
					this);
			try {
				npp.store((IEclipsePreferences) node.node("test"));
			} catch (final BackingStoreException e) {
				e.printStackTrace();
			}
		}

		node.removePreferenceChangeListener(this);

		try {
			node.put(ProjectPreferencesConstants.SOURCE_DIRS, sourceDirs);
			node.put(ProjectPreferencesConstants.INCLUDE_DIRS, includeDirs);
			node.put(ProjectPreferencesConstants.OUTPUT_DIR, outputDir);
			node.put(ProjectPreferencesConstants.USE_PATHZ, usePathZ);
			node.put(ProjectPreferencesConstants.EXTERNAL_INCLUDES,
					externalIncludesFile);
			if (runtimeVersion.isDefined()) {
				node.put(ProjectPreferencesConstants.RUNTIME_VERSION,
						runtimeVersion.asMinor().toString());
			} else {
				node.remove(ProjectPreferencesConstants.RUNTIME_VERSION);
			}
			if (runtimeName != null) {
				node.put(ProjectPreferencesConstants.RUNTIME_NAME, runtimeName);
			} else {
				node.remove(ProjectPreferencesConstants.RUNTIME_NAME);
			}
			// TODO remove these later
			node.remove("backend_cookie");
			node.remove("backend_node");
			// end todo
			node.put(ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
					externalModulesFile);

			try {
				node.flush();
			} catch (final BackingStoreException e1) {
			}
		} finally {
			node.addPreferenceChangeListener(this);
		}
	}

	public String getIncludeDirsString() {
		return includeDirs;
	}

	public void setIncludeDirsString(final String dirs) {
		includeDirs = dirs;
	}

	public String[] getIncludeDirs() {
		return PreferencesUtils.unpackArray(includeDirs);
	}

	public void setIncludeDirs(final String[] dirs) {
		includeDirs = PreferencesUtils.packArray(dirs);
	}

	public String getOutputDir() {
		return outputDir;
	}

	public void setOutputDir(final String dir) {
		if (!outputDir.equals(dir)) {
			// try {
			// final Backend b = ErlangCore.getBackendManager()
			// .getBuildBackend(project);
			// String p =
			// project.getLocation().append(outputDir).toString();
			// b.removePath(getUsePathZ(), p);
			//
			// p = project.getLocation().append(dir).toString();
			// b.addPath(getUsePathZ(), p);

			outputDir = dir;
			// } catch (final BackendException e) {
			// ErlLogger.warn(e);
			// }
		}
	}

	public boolean getUsePathZ() {
		return Boolean.parseBoolean(usePathZ);
	}

	public void setUsePathZ(final boolean pz) {
		final boolean z = Boolean.parseBoolean(usePathZ);
		if (z != pz) {
			for (final FullBackend b : ErlangCore.getBackendManager()
					.getExecutionBackends(project)) {

				final String p = project.getLocation().append(outputDir)
						.toString();
				b.removePath(z, p);
				b.addPath(pz, p);
			}
		}
		usePathZ = Boolean.toString(pz);
	}

	public String getSourceDirsString() {
		return sourceDirs;
	}

	public void setSourceDirsString(final String dirs) {
		sourceDirs = dirs;
	}

	public String[] getSourceDirs() {
		return PreferencesUtils.unpackArray(sourceDirs);
	}

	public void setSourceDirs(final String[] dirs) {
		sourceDirs = PreferencesUtils.packArray(dirs);
	}

	public String buildCommandLine() {
		if (project != null) {
			final String incs = buildIncludeDirs(getIncludeDirs());
			return " -pa " + project.getLocation().append(outputDir) + incs;
		}
		return "";
	}

	public String buildIncludeDirs(final String[] dirs) {
		final StringBuilder incs = new StringBuilder();
		for (final String element : dirs) {
			final IPath loc = project.getLocation();
			IPath inc = new Path(element);
			ErlLogger.debug("* " + inc);
			if (!inc.isAbsolute()) {
				ErlLogger.debug("  not abs!");
				inc = loc.append(inc);
				ErlLogger.debug("  " + inc);
			}
			incs.append(" -I").append(inc.toString());
		}
		return incs.toString();
	}

	public void copyFrom(final ErlangProjectProperties bprefs) {
		includeDirs = bprefs.includeDirs;
		sourceDirs = bprefs.sourceDirs;
		outputDir = bprefs.outputDir;
	}

	public String getExternalIncludesFile() {
		return externalIncludesFile;
	}

	public void setExternalIncludesFile(final String file) {
		externalIncludesFile = file;
	}

	public IProject getProject() {
		return project;
	}

	public void setExternalModulesFile(final String externalModules) {
		this.externalModulesFile = externalModules;
	}

	public String getExternalModulesFile() {
		return externalModulesFile;
	}

	public RuntimeInfo getRuntimeInfo() {
		final RuntimeInfo runtime = ErlangCore.getRuntimeInfoManager()
				.getRuntime(runtimeVersion, runtimeName);
		RuntimeInfo rt = null;
		if (runtime != null) {
			rt = RuntimeInfo.copy(runtime, false);
		}
		return rt;
	}

	public boolean hasSourceDir(final IPath fullPath) {
		final String f = fullPath.removeFirstSegments(1).toString();
		for (final String s : getSourceDirs()) {
			if (s.equals(f)) {
				return true;
			}
		}
		return false;
	}

	public RuntimeVersion getRuntimeVersion() {
		return runtimeVersion;
	}

	public void preferenceChange(final PreferenceChangeEvent event) {
		final IEclipsePreferences root = new ProjectScope(project)
				.getNode(ErlangPlugin.PLUGIN_ID);
		load(root);
		// System.out.println("!!! project preferences " + event.getNode() +
		// ": "
		// + event.getKey() + " " + event.getOldValue() + " "
		// + event.getNewValue() + " ... " + event.getSource());
	}

	public void setRuntimeVersion(final RuntimeVersion runtimeVersion) {
		this.runtimeVersion = runtimeVersion;
	}
}
