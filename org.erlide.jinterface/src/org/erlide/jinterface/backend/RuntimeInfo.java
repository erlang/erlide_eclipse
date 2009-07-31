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
package org.erlide.jinterface.backend;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class RuntimeInfo {
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
	private String suffix = "";
	private boolean useLongName = true;
	private boolean startShell = false;

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
		rt.version = o.version;
		rt.useLongName = o.useLongName;
		rt.startShell = o.startShell;
		return rt;
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
		return this.nodeName + suffix;
	}

	public void setNodeNameSuffix(String suffix) {
		this.suffix = suffix;
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
				getNodeName(), getOtpHome(), version, getArgs());
	}

	public String getCmdLine() {
		final String pathA = cvt(getPathA());
		final String pathZ = cvt(getPathZ());
		String msg = "%s %s %s %s";
		String erl = getOtpHome() + "/bin/erl";
		if (erl.indexOf(' ') >= 0) {
			erl = "\"" + erl + "\"";
		}
		String pa = ifNotEmpty("-pa ", pathA);
		String pz = ifNotEmpty("-pz ", pathZ);
		String cmd = String.format(msg, erl, pa, pz, getArgs());
		String cky = getCookie();
		if (!startShell) {
			cmd += " -noshell";
		}
		cky = cky == null ? "" : " -setcookie " + cky;
		final boolean globalLongName = System.getProperty("erlide.longname",
				"none").equals("true");
		final String nameTag = (useLongName || globalLongName) ? " -name "
				: " -sname ";
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

	public String getOtpHome() {
		return homeDir;
	}

	public void setOtpHome(final String otpHome) {
		homeDir = otpHome;
		version = RuntimeVersion.getVersion(otpHome);
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
		final String v = RuntimeVersion.getRuntimeVersion(path);
		return v != null;
	}

	static String readstring(final InputStream is) {
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

	public RuntimeVersion getVersion() {
		return version;
	}

	public void useLongName(boolean longName) {
		useLongName = longName;
	}

	public boolean getLongName() {
		return useLongName;

	}

	public void setStartShell(boolean startShell) {
		this.startShell = startShell;
	}

	public boolean isStartShell() {
		return startShell;
	}
}
