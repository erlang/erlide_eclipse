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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BuildBackend;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.RuntimeInfoManager;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.osgi.service.prefs.BackingStoreException;

public class ErlangProjectProperties {

	private static final String PATH_SEP = ";";

	private IProject project;

	private String fSourceDirs = IPrefConstants.DEFAULT_SOURCE_DIRS;
	private String fUsePathZ = IPrefConstants.DEFAULT_USE_PATHZ;
	private String fOutputDir = IPrefConstants.DEFAULT_OUTPUT_DIR;
	private String fIncludeDirs = IPrefConstants.DEFAULT_INCLUDE_DIRS;
	private String fExternalIncludes = IPrefConstants.DEFAULT_EXTERNAL_INCLUDES;
	private String fExternalModules = IPrefConstants.DEFAULT_EXTERNAL_MODULES;

	private String fRuntimeName;
	private String fNodeName;
	private String fCookie;

	private boolean fUnique = true;

	/**
	 * Name of file containing project classpath
	 */
	public static final String CODEPATH_FILENAME = ".codepath"; //$NON-NLS-1$

	public ErlangProjectProperties() {
		fRuntimeName = IPrefConstants.DEFAULT_RUNTIME_NAME;
	}

	public ErlangProjectProperties(IProject prj) {
		super();
		project = prj;
		load();
	}

	public void load() {
		if (project == null) {
			return;
		}

		// if .codepath exists, read from it, otherwise from .settings

		boolean loaded = false;
		final IFile cp = project.getFile(CODEPATH_FILENAME);
		if (cp.exists()) {
			final File codepath = cp.getRawLocation().toFile();
			final Properties prefs = new Properties();
			FileInputStream stream;
			try {
				stream = new FileInputStream(codepath);
				prefs.load(stream);
				stream.close();
				loaded = true;
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}

			fSourceDirs = prefs.getProperty(IPrefConstants.SOURCE_DIRS,
					IPrefConstants.DEFAULT_SOURCE_DIRS);
			fIncludeDirs = prefs.getProperty(IPrefConstants.INCLUDE_DIRS,
					IPrefConstants.DEFAULT_INCLUDE_DIRS);
			fOutputDir = prefs.getProperty(IPrefConstants.OUTPUT_DIR,
					IPrefConstants.DEFAULT_OUTPUT_DIR);
			fUsePathZ = prefs.getProperty(IPrefConstants.USE_PATHZ,
					IPrefConstants.DEFAULT_USE_PATHZ);
			fExternalIncludes = prefs.getProperty(
					IPrefConstants.EXTERNAL_INCLUDES,
					IPrefConstants.DEFAULT_EXTERNAL_INCLUDES);
			fRuntimeName = prefs.getProperty(IPrefConstants.RUNTIME_NAME,
					IPrefConstants.DEFAULT_RUNTIME_NAME);
			fNodeName = prefs.getProperty(IPrefConstants.NODE_NAME,
					IPrefConstants.DEFAULT_NODENAME);
			fUnique = Boolean.parseBoolean(prefs.getProperty(
					IPrefConstants.MK_UNIQUE, "true"));
			fCookie = prefs.getProperty(IPrefConstants.COOKIE,
					IPrefConstants.DEFAULT_COOKIE);
			fExternalModules = prefs.getProperty(
					IPrefConstants.PROJECT_EXTERNAL_MODULES,
					IPrefConstants.DEFAULT_EXTERNAL_MODULES);
		}
		if (!loaded) {
			// ErlLogger.debug("project %s, loading from .settings", project
			// .getName());

			ProjectScope s = new ProjectScope(project);
			IEclipsePreferences node = s.getNode(ErlangLaunchPlugin.PLUGIN_ID);

			// new settings
			fSourceDirs = node.get(IPrefConstants.SOURCE_DIRS,
					IPrefConstants.DEFAULT_SOURCE_DIRS);
			fIncludeDirs = node.get(IPrefConstants.INCLUDE_DIRS,
					IPrefConstants.DEFAULT_INCLUDE_DIRS);
			fOutputDir = node.get(IPrefConstants.OUTPUT_DIR,
					IPrefConstants.DEFAULT_OUTPUT_DIR);
			fUsePathZ = node.get(IPrefConstants.USE_PATHZ,
					IPrefConstants.DEFAULT_USE_PATHZ);
			fExternalIncludes = node.get(IPrefConstants.EXTERNAL_INCLUDES,
					IPrefConstants.DEFAULT_EXTERNAL_INCLUDES);
			fRuntimeName = node.get(IPrefConstants.RUNTIME_NAME,
					IPrefConstants.DEFAULT_RUNTIME_NAME);
			fNodeName = node.get(IPrefConstants.NODE_NAME,
					IPrefConstants.DEFAULT_NODENAME);
			fUnique = Boolean.parseBoolean(node.get(IPrefConstants.MK_UNIQUE,
					"true"));
			fCookie = node.get(IPrefConstants.COOKIE,
					IPrefConstants.DEFAULT_COOKIE);
			fExternalModules = node.get(
					IPrefConstants.PROJECT_EXTERNAL_MODULES,
					IPrefConstants.DEFAULT_EXTERNAL_MODULES);
		}
	}

	public void store() {
		if (project == null) {
			return;
		}

		// save in .settings
		// ErlLogger.debug("project %s, saving to .settings",
		// project.getName());

		ProjectScope s = new ProjectScope(project);
		IEclipsePreferences node = s.getNode(ErlangLaunchPlugin.PLUGIN_ID);

		node.put(IPrefConstants.SOURCE_DIRS, fSourceDirs);
		node.put(IPrefConstants.INCLUDE_DIRS, fIncludeDirs);
		node.put(IPrefConstants.OUTPUT_DIR, fOutputDir);
		node.put(IPrefConstants.USE_PATHZ, fUsePathZ);
		node.put(IPrefConstants.EXTERNAL_INCLUDES, fExternalIncludes);
		node.put(IPrefConstants.RUNTIME_NAME, fRuntimeName);
		node.put(IPrefConstants.NODE_NAME, fNodeName);
		node.put(IPrefConstants.MK_UNIQUE, Boolean.toString(fUnique));
		node.put(IPrefConstants.COOKIE, fCookie);
		node.put(IPrefConstants.PROJECT_EXTERNAL_MODULES, fExternalModules);

		try {
			node.flush();
		} catch (BackingStoreException e1) {
		}

		final IFile cp = project.getFile(CODEPATH_FILENAME);
		if (cp.exists()) {
			// save in .codepath
			// ErlLogger.debug("project %s, saving to .codepath", project
			// .getName());

			final File codepath = cp.getRawLocation().toFile();
			final Properties prefs = new Properties();

			prefs.put(IPrefConstants.SOURCE_DIRS, fSourceDirs);
			prefs.put(IPrefConstants.INCLUDE_DIRS, fIncludeDirs);
			prefs.put(IPrefConstants.OUTPUT_DIR, fOutputDir);
			prefs.put(IPrefConstants.USE_PATHZ, fUsePathZ);
			prefs.put(IPrefConstants.EXTERNAL_INCLUDES, fExternalIncludes);
			prefs.put(IPrefConstants.RUNTIME_NAME, fRuntimeName);
			prefs.put(IPrefConstants.NODE_NAME, fNodeName);
			prefs.put(IPrefConstants.MK_UNIQUE, Boolean.toString(fUnique));
			prefs.put(IPrefConstants.COOKIE, fCookie);
			prefs
					.put(IPrefConstants.PROJECT_EXTERNAL_MODULES,
							fExternalModules);

			try {
				FileOutputStream out = new FileOutputStream(codepath);
				try {
					prefs.store(out, null);
				} finally {
					out.close();
				}
			} catch (final IOException e) {
			}
		}
	}

	public String getIncludeDirsString() {
		return fIncludeDirs;
	}

	public void setIncludeDirsString(String includeDirs) {
		fIncludeDirs = includeDirs;
	}

	public String[] getIncludeDirs() {
		return unpack(fIncludeDirs);
	}

	public void setIncludeDirs(String[] includeDirs) {
		fIncludeDirs = pack(includeDirs);
	}

	public String getOutputDir() {
		return fOutputDir;
	}

	public void setOutputDir(String outputDir) {
		if (!fOutputDir.equals(outputDir)) {
			try {
				BuildBackend b = BackendManager.getDefault().getBuild(project);
				String p = project.getLocation().append(fOutputDir).toString();
				b.removePath(getUsePathZ(), p);

				p = project.getLocation().append(outputDir).toString();
				b.addPath(getUsePathZ(), p);
			} catch (BackendException e) {
				ErlLogger.info(e);
			}

		}
		fOutputDir = outputDir;
	}

	public boolean getUsePathZ() {
		return Boolean.parseBoolean(fUsePathZ);
	}

	public void setUsePathZ(boolean pz) {
		boolean z = Boolean.parseBoolean(fUsePathZ);
		if (z != pz) {
			for (IBackend b : BackendManager.getDefault().getExecution(project)) {

				String p = project.getLocation().append(fOutputDir).toString();
				b.removePath(z, p);
				b.addPath(pz, p);
			}
		}
		fUsePathZ = Boolean.toString(pz);
	}

	public String getSourceDirsString() {
		return fSourceDirs;
	}

	public void setSourceDirsString(String sourceDirs) {
		fSourceDirs = sourceDirs;
	}

	public String[] getSourceDirs() {
		return unpack(fSourceDirs);
	}

	public void setSourceDirs(String[] sourceDirs) {
		fSourceDirs = pack(sourceDirs);
	}

	public String buildCommandLine() {
		if (project != null) {
			final String incs = buildIncludeDirs(getIncludeDirs());
			return " -pa " + project.getLocation().append(fOutputDir) + incs;
		}
		return "";
	}

	public String buildIncludeDirs(String[] dirs) {
		final StringBuilder incs = new StringBuilder();
		for (String element : dirs) {
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

	public void copyFrom(ErlangProjectProperties bprefs) {
		fIncludeDirs = bprefs.fIncludeDirs;
		fSourceDirs = bprefs.fSourceDirs;
		fOutputDir = bprefs.fOutputDir;
		fRuntimeName = "";
	}

	public static String pack(String[] strs) {
		final StringBuilder b = new StringBuilder();
		for (int i = 0; i < strs.length; i++) {
			b.append(strs[i]);
			if (i < strs.length - 1) {
				b.append(PATH_SEP);
			}
		}
		return b.toString();
	}

	public String[] getExternalIncludes() {
		return unpack(fExternalIncludes);
	}

	private String[] unpack(String str) {
		final String[] res = str.split(PATH_SEP);
		for (int i = 0; i < res.length; i++) {
			res[i] = res[i].trim();
		}
		return res;
	}

	public String getExternalIncludesString() {
		return fExternalIncludes;
	}

	public void setExternalIncludes(String[] externalIncludes) {
		fExternalIncludes = pack(externalIncludes);
	}

	public IProject getProject() {
		return project;
	}

	public void setRuntimeName(String backendName) {
		// TODO validate!
		fRuntimeName = backendName;
	}

	public void setExternalModules(String fExternalModules) {
		this.fExternalModules = fExternalModules;
	}

	public String getExternalModules() {
		return fExternalModules;
	}

	public String getRuntimeName() {
		return fRuntimeName;
	}

	public RuntimeInfo getRuntimeInfo() {
		RuntimeInfo rt = RuntimeInfo.copy(RuntimeInfoManager.getDefault()
				.getRuntime(fRuntimeName), false);
		if (rt != null) {
			rt.setNodeName(fNodeName);
			rt.setUniqueName(fUnique);
			rt.setCookie(fCookie);
		}
		return rt;
	}

	public void setCookie(String text) {
		fCookie = text.trim();
	}

	public String getCookie() {
		return fCookie;
	}

	public String getNodeName() {
		return fNodeName;
	}

	public void setNodeName(String text) {
		fNodeName = text.trim();
	}

	public void setUniqueName(boolean unique) {
		fUnique = unique;
	}

	public boolean isUniqueName() {
		return this.fUnique;
	}

	public boolean hasSourceDir(IPath fullPath) {
		for (String s : getSourceDirs()) {
			if (s.equals(fullPath)) {
				return true;
			}
		}
		return false;
	}

}
