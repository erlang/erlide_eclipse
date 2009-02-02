/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.runtime;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.runtime.backend.Backend;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.osgi.service.prefs.BackingStoreException;

public class ErlangProjectProperties {

	private static final String PATH_SEP = ";";

	private IProject project;

	private String sourceDirs = ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS;
	private String usePathZ = ProjectPreferencesConstants.DEFAULT_USE_PATHZ;
	private String outputDir = ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR;
	private String includeDirs = ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS;
	private String externalIncludes = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
	private String externalModules = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;

	private String runtimeVersion;
	private String runtimeName;
	private String nodeName;
	private String cookie;

	private boolean fUnique = true;

	public enum ProjectType {
		NORMAL, REFERENCE, OTP
	};

	private ProjectType type = ProjectType.NORMAL;

	private boolean saveRuntimeName;

	public static final String CODEPATH_FILENAME = ".codepath"; //$NON-NLS-1$

	public ErlangProjectProperties() {
		runtimeName = ProjectPreferencesConstants.DEFAULT_RUNTIME_NAME;
	}

	public ErlangProjectProperties(final IProject prj) {
		super();
		project = prj;
		// TODO load() should not be in constructor!
		load();
	}

	public ErlangProjectProperties(final IProject prj, final ProjectType type) {
		super();
		project = prj;
		// TODO load() should not be in constructor!
		load();
		this.type = type;
	}

	public ErlangProjectProperties load() {
		if (project == null) {
			return this;
		}

		final IFile cp = project.getFile(CODEPATH_FILENAME);
		if (cp.exists()) {
			String msg = "Found old configuration file %s for project %s, please remove it.";
			ErlLogger.warn(msg, CODEPATH_FILENAME, project.getName());
		}

		final ProjectScope s = new ProjectScope(project);
		final IEclipsePreferences node = s.getNode(ErlangPlugin.PLUGIN_ID);

		sourceDirs = node.get(ProjectPreferencesConstants.SOURCE_DIRS,
				ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
		includeDirs = node.get(ProjectPreferencesConstants.INCLUDE_DIRS,
				ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
		outputDir = node.get(ProjectPreferencesConstants.OUTPUT_DIR,
				ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
		usePathZ = node.get(ProjectPreferencesConstants.USE_PATHZ,
				ProjectPreferencesConstants.DEFAULT_USE_PATHZ);
		runtimeVersion = node.get(ProjectPreferencesConstants.RUNTIME_VERSION,
				null);
		runtimeName = node.get(ProjectPreferencesConstants.RUNTIME_NAME, null);
		if (runtimeName != null && runtimeName.length() != 0) {
			saveRuntimeName = true;
			ErlLogger.debug("Runtime name specified: %s", runtimeName);
			RuntimeInfo runtime = ErlangCore.getRuntimeInfoManager()
					.getRuntime(runtimeName);
			if (runtime != null) {
				// fRuntimeVersion = runtime.getVersion();
			}
		} else if (runtimeVersion != null) {
			saveRuntimeName = false;
			List<RuntimeInfo> runtimes = ErlangCore.getRuntimeInfoManager()
					.locateVersion(runtimeVersion);
			if (runtimes.size() > 0) {
				runtimeName = runtimes.get(0).getName();
				ErlLogger.debug("Runtime name located for version %s: %s",
						runtimeVersion, runtimeName);
			} else {
				runtimeName = ProjectPreferencesConstants.DEFAULT_RUNTIME_NAME;
				ErlLogger.debug(
						"Missing runtime name and version, using default: %s",
						runtimeName);
			}
		} else {
			saveRuntimeName = false;
			runtimeName = ProjectPreferencesConstants.DEFAULT_RUNTIME_NAME;
			ErlLogger.debug(
					"Missing runtime name and version, using default: %s",
					runtimeName);
		}
		nodeName = node.get(ProjectPreferencesConstants.NODE_NAME,
				ProjectPreferencesConstants.DEFAULT_NODENAME);
		fUnique = Boolean.parseBoolean(node.get(
				ProjectPreferencesConstants.MK_UNIQUE, "true"));
		cookie = node.get(ProjectPreferencesConstants.COOKIE,
				ProjectPreferencesConstants.DEFAULT_COOKIE);
		externalModules = node.get(
				ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
				ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES);
		externalIncludes = node.get(
				ProjectPreferencesConstants.EXTERNAL_INCLUDES,
				ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES);
		type = ProjectType.valueOf(node.get(
				ProjectPreferencesConstants.PROJECT_TYPE,
				ProjectPreferencesConstants.DEFAULT_PROJECT_TYPE));
		return this;
	}

	public void store() {
		if (project == null) {
			return;
		}

		final ProjectScope s = new ProjectScope(project);
		final IEclipsePreferences node = s.getNode(ErlangPlugin.PLUGIN_ID);

		node.put(ProjectPreferencesConstants.SOURCE_DIRS, sourceDirs);
		node.put(ProjectPreferencesConstants.INCLUDE_DIRS, includeDirs);
		node.put(ProjectPreferencesConstants.OUTPUT_DIR, outputDir);
		node.put(ProjectPreferencesConstants.USE_PATHZ, usePathZ);
		node.put(ProjectPreferencesConstants.EXTERNAL_INCLUDES,
				externalIncludes);
		if (runtimeVersion != null) {
			node.put(ProjectPreferencesConstants.RUNTIME_VERSION,
					runtimeVersion);
		}
		if (runtimeName != null && saveRuntimeName) {
			node.put(ProjectPreferencesConstants.RUNTIME_NAME, runtimeName);
		} else {
			node.remove(ProjectPreferencesConstants.RUNTIME_NAME);
		}
		node.put(ProjectPreferencesConstants.NODE_NAME, nodeName);
		node.put(ProjectPreferencesConstants.MK_UNIQUE, Boolean
				.toString(fUnique));
		node.put(ProjectPreferencesConstants.COOKIE, cookie);
		node.put(ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
				externalModules);
		node.put(ProjectPreferencesConstants.PROJECT_TYPE, type.toString());

		try {
			node.flush();
		} catch (final BackingStoreException e1) {
		}

	}

	public String getIncludeDirsString() {
		return includeDirs;
	}

	public void setIncludeDirsString(final String dirs) {
		includeDirs = dirs;
	}

	public String[] getIncludeDirs() {
		return unpack(includeDirs);
	}

	public void setIncludeDirs(final String[] dirs) {
		includeDirs = pack(dirs);
	}

	public String getOutputDir() {
		return outputDir;
	}

	public void setOutputDir(final String dir) {
		if (!outputDir.equals(dir)) {
			try {
				final Backend b = ErlangCore.getBackendManager()
						.getBuildBackend(project);
				String p = project.getLocation().append(outputDir).toString();
				b.removePath(getUsePathZ(), p);

				p = project.getLocation().append(dir).toString();
				b.addPath(getUsePathZ(), p);

				outputDir = dir;
			} catch (final BackendException e) {
				ErlLogger.warn(e);
			}
		}
	}

	public boolean getUsePathZ() {
		return Boolean.parseBoolean(usePathZ);
	}

	public void setUsePathZ(final boolean pz) {
		final boolean z = Boolean.parseBoolean(usePathZ);
		if (z != pz) {
			for (final Backend b : ErlangCore.getBackendManager()
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
		return unpack(sourceDirs);
	}

	public void setSourceDirs(final String[] dirs) {
		sourceDirs = pack(dirs);
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
		runtimeName = "";
	}

	public static String pack(final String[] strs) {
		final StringBuilder b = new StringBuilder();
		for (int i = 0; i < strs.length; i++) {
			if (strs[i] != null && strs[i].length() > 0) {
				b.append(strs[i]);
				b.append(PATH_SEP);
			}
		}
		if (b.length() > 0) {
			b.deleteCharAt(b.length() - 1);
		}
		return b.toString();
	}

	public String[] getExternalIncludes() {
		return unpack(externalIncludes);
	}

	private String[] unpack(final String str) {
		final String[] res = str.split(PATH_SEP);
		for (int i = 0; i < res.length; i++) {
			res[i] = res[i].trim();
		}
		return res;
	}

	public String getExternalIncludesString() {
		return externalIncludes;
	}

	public void setExternalIncludes(final String[] externalIncludes) {
		final String packed = pack(externalIncludes);
		setExternalIncludes(packed);
	}

	/**
	 * @param packed
	 */
	public void setExternalIncludes(final String packed) {
		externalIncludes = packed;
	}

	public IProject getProject() {
		return project;
	}

	public void setRuntimeName(final String backendName) {
		// TODO validate!
		runtimeName = backendName;
	}

	public void setExternalModules(final String fExternalModules) {
		this.externalModules = fExternalModules;
	}

	public String getExternalModules() {
		return externalModules;
	}

	public String getRuntimeName() {
		return runtimeName;
	}

	public RuntimeInfo getRuntimeInfo() {
		final RuntimeInfo rt = RuntimeInfo.copy(ErlangCore
				.getRuntimeInfoManager().getRuntime(runtimeName), false);
		if (rt != null) {
			rt.setNodeName(nodeName);
			rt.setUniqueName(fUnique);
			rt.setCookie(cookie);
		}
		return rt;
	}

	public void setCookie(final String text) {
		cookie = text.trim();
	}

	public String getCookie() {
		return cookie;
	}

	public String getNodeName() {
		return nodeName;
	}

	public void setNodeName(final String text) {
		nodeName = text.trim();
	}

	public void setUniqueName(final boolean unique) {
		fUnique = unique;
	}

	public boolean isUniqueName() {
		return fUnique;
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

	public boolean isOtp() {
		return type == ProjectType.OTP;
	}

	public boolean isReference() {
		return type != ProjectType.NORMAL;
	}

	public boolean isNormal() {
		return type == ProjectType.NORMAL;
	}

	public void setType(final ProjectType type) {
		this.type = type;
	}

	public String getRuntimeVersion() {
		return runtimeVersion;
	}

}
