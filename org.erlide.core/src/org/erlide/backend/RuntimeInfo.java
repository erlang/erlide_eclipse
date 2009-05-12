/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.erlide.core.preferences.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public class RuntimeInfo {
	static final String CODE_PATH = "codePath";
	static final String HOME_DIR = "homeDir";
	static final String ARGS = "args";
	static final String WORKING_DIR = "workingDir";
	static final String MANAGED = "managed";

	public static final String DEFAULT_MARKER = "*DEFAULT*";

	private String homeDir = "";
	private String args = "";
	private String cookie = "";
	private String nodeName = "";
	private String workingDir = "";
	private boolean managed; // will it be started/stopped by us?

	private RuntimeVersion version;
	private String name;
	private List<String> codePath;
	private boolean unique = false;

	public RuntimeInfo() {
		super();
		codePath = new ArrayList<String>();
		codePath.add(DEFAULT_MARKER);
	}

	public static RuntimeInfo copy(final RuntimeInfo o, final boolean mkCopy) {
		if (o == null) {
			return null;
		}
		final RuntimeInfo rt = new RuntimeInfo();
		rt.name = o.name;
		if (mkCopy) {
			rt.name += "_copy";
		}
		rt.args = o.args;
		rt.codePath = new ArrayList<String>(o.codePath);
		rt.managed = o.managed;
		rt.homeDir = o.homeDir;
		rt.workingDir = o.workingDir;
		rt.nodeName = o.nodeName;
		return rt;
	}

	public void store(final Preferences root) {
		final Preferences node = root.node(getName());
		final String code = PreferencesUtils.packList(getCodePath());
		node.put(CODE_PATH, code);
		node.put(HOME_DIR, getOtpHome());
		node.put(ARGS, args);
		node.put(WORKING_DIR, workingDir);
		node.putBoolean(MANAGED, managed);
	}

	public void load(final Preferences node) {
		setName(node.name());
		final String path = node.get(CODE_PATH, "");
		setCodePath(PreferencesUtils.unpackList(path));
		setOtpHome(node.get(HOME_DIR, ""));
		args = node.get(ARGS, "");
		final String wd = node.get(WORKING_DIR, workingDir);
		if (wd.length() != 0) {
			workingDir = wd;
		}
		managed = node.getBoolean(MANAGED, true);
	}

	public String getArgs() {
		return this.args;
	}

	public void setArgs(final String args) {
		this.args = args;
	}

	public String getCookie() {
		if ("".equals(cookie)) {
			cookie = null;
		}
		return this.cookie;
	}

	public void setCookie(final String cookie) {
		this.cookie = cookie;
	}

	public String getNodeName() {
		final String suffix = unique ? "_"
				+ BackendUtil.getErlideNameSuffix() : "";
		return this.nodeName + suffix;
	}

	public void setNodeName(final String nodeName) {
		if (validateNodeName(nodeName)) {
			this.nodeName = nodeName;
		} else {
			this.nodeName = nodeName.replaceAll("[^a-zA-Z0-9_-]", "");
		}
	}

	public boolean isManaged() {
		return this.managed;
	}

	public void setManaged(final boolean managed) {
		this.managed = managed;
	}

	public List<String> getPathA() {
		return getPathA(DEFAULT_MARKER);
	}

	public List<String> getPathZ() {
		return getPathZ(DEFAULT_MARKER);
	}

	public String getWorkingDir() {
		return (workingDir == null || workingDir.length() == 0) ? "."
				: workingDir;
	}

	public void setWorkingDir(final String workingDir) {
		this.workingDir = workingDir;
	}

	@Override
	public String toString() {
		return String.format("Backend<%s/%s (%s) %s [%s]>", getName(),
				getNodeName(), getOtpHome(), getVersion(), getArgs());
	}

	public String getCmdLine() {
		final String pathA = cvt(getPathA());
		final String pathZ = cvt(getPathZ());
		String cmd = String.format("%s/bin/erl %s %s %s", getOtpHome(),
				ifNotEmpty(" -pa ", pathA), ifNotEmpty(" -pz ", pathZ),
				getArgs());
		String cky = getCookie();
		cky = cky == null ? "" : " -setcookie " + cky;
		final boolean useLongName = System.getProperty("erlide.longname",
				"true").equals("true");
		final String nameTag = useLongName ? " -name " : " -sname ";
		cmd += nameTag + BackendUtil.buildNodeName(getNodeName(), useLongName)
				+ cky;
		return cmd;
	}

	private String ifNotEmpty(final String key, final String str) {
		if (str == null || str.length() == 0) {
			return "";
		}
		return key + str;
	}

	public RuntimeVersion getVersion() {
		if (version == null) {
			version = new RuntimeVersion(getRuntimeVersion(homeDir));
		}
		return version;
	}

	public String getOtpHome() {
		return homeDir;
	}

	public void setOtpHome(final String otpHome) {
		homeDir = otpHome;
		getVersion();
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public List<String> getCodePath() {
		return codePath;
	}

	public void setCodePath(final List<String> path) {
		codePath = path;
	}

	protected List<String> getPathA(final String marker) {
		if (codePath != null) {
			final List<String> list = codePath;
			final int i = list.indexOf(marker);
			if (i < 0) {
				return list;
			}
			return list.subList(0, i);
		}
		return Collections.emptyList();
	}

	protected List<String> getPathZ(final String marker) {
		if (codePath != null) {
			final List<String> list = codePath;
			final int i = list.indexOf(marker);
			if (i < 0) {
				return Collections.emptyList();
			}
			return list.subList(i + 1, codePath.size());
		}
		return Collections.emptyList();
	}

	public static boolean validateNodeName(final String name) {
		return name != null
				&& name.matches("[a-zA-Z0-9_-]+(@[a-zA-Z0-9_.-]+)?");
	}

	public static boolean validateLocation(final String path) {
		final String v = getRuntimeVersion(path);
		return v != null;
	}

	public static String getRuntimeVersion(final String path) {
		if (path == null) {
			return null;
		}
		String result = null;
		final File boot = new File(path + "/bin/start.boot");
		try {
			final FileInputStream is = new FileInputStream(boot);
			is.skip(14);
			readstring(is);
			result = readstring(is);

			// now get minor version from kernel's minor version
			final File lib = new File(path + "/lib");
			final File[] kernels = lib.listFiles(new FileFilter() {
				public boolean accept(final File pathname) {
					try {
						boolean r = pathname.isDirectory();
						r &= pathname.getName().startsWith("kernel-");
						final String canonicalPath = pathname
								.getCanonicalPath().toLowerCase();
						final String absolutePath = pathname.getAbsolutePath()
								.toLowerCase();
						r &= canonicalPath.equals(absolutePath);
						return r;
					} catch (final IOException e) {
						return false;
					}
				}
			});
			if (kernels != null && kernels.length > 0) {
				final int[] krnls = new int[kernels.length];
				for (int i = 0; i < kernels.length; i++) {
					final String k = kernels[i].getName();
					try {
						int p = k.indexOf('.');
						if (p < 0) {
							krnls[i] = 0;
						} else {
							p = k.indexOf('.', p + 1);
							if (p < 0) {
								krnls[i] = 0;
							} else {
								krnls[i] = Integer.parseInt(k.substring(p + 1));
							}
						}
					} catch (final Exception e) {
						krnls[i] = 0;
					}
				}
				Arrays.sort(krnls);
				final String ver = Integer.toString(krnls[krnls.length - 1]);
				result += "-" + ver;
			}
		} catch (final IOException e) {
		}
		return result;
	}

	private static String readstring(final InputStream is) {
		try {
			is.read();
			byte[] b = new byte[2];
			is.read(b);
			final int len = b[0] * 256 + b[1];
			b = new byte[len];
			is.read(b);
			final String s = new String(b);
			return s;
		} catch (final IOException e) {
			return null;
		}
	}

	public static boolean isValidOtpHome(final String otpHome) {
		// Check if it looks like a ERL_TOP location:
		if (otpHome == null) {
			return false;
		}
		if (otpHome.length() == 0) {
			return false;
		}
		final File d = new File(otpHome);
		if (!d.isDirectory()) {
			return false;
		}

		final File erl = new File(otpHome + "/bin/erl");
		final File erlexe = new File(otpHome + "/bin/erl.exe");
		final boolean hasErl = erl.exists() || erlexe.exists();

		final File lib = new File(otpHome + "/lib");
		final boolean hasLib = lib.isDirectory() && lib.exists();

		return hasErl && hasLib;
	}

	public static boolean hasCompiler(final String otpHome) {
		// Check if it looks like a ERL_TOP location:
		if (otpHome == null) {
			return false;
		}
		if (otpHome.length() == 0) {
			return false;
		}
		final File d = new File(otpHome);
		if (!d.isDirectory()) {
			return false;
		}

		final File erlc = new File(otpHome + "/bin/erlc");
		final File erlcexe = new File(otpHome + "/bin/erlc.exe");
		final boolean hasErlc = erlc.exists() || erlcexe.exists();

		return hasErlc;
	}

	protected static String cvt(final Collection<String> path) {
		String result = "";
		for (String s : path) {
			if (s.length() > 0) {
				if (s.contains(" ")) {
					s = "\"" + s + "\"";
				}
				result += s + ";";
			}
		}
		return result;
	}

	public void setUniqueName(final boolean unique) {
		this.unique = unique;
	}

}
