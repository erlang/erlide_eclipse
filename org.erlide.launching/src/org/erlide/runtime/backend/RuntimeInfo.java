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
package org.erlide.runtime.backend;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.erlide.runtime.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public class RuntimeInfo implements Cloneable {
	private static final String CODE_PATH = "codePath";
	private static final String HOME_DIR = "homeDir";
	private static final String NODENAME = "nodeName";
	private static final String COOKIE = "cookie";
	private static final String ARGS = "args";
	private static final String WORKING_DIR = "workingDir";
	private static final String MANAGED = "managed";
	private static final String ERLIDE = "erlide";

	public static final String DEFAULT_MARKER = "*DEFAULT*";

	private String homeDir = "";
	private String args = "";
	private String cookie = "";
	private String nodeName = "";
	private String workingDir = "";
	private boolean managed; // will it be started/stopped by us?
	private boolean erlide; // will erlide code be installed?

	private String fVersion;
	private String name;
	private List<String> codePath;

	public RuntimeInfo() {
		super();
		codePath = new ArrayList<String>();
		codePath.add(DEFAULT_MARKER);
	}

	public RuntimeInfo(RuntimeInfo o) {
		super();
		name = o.name + "_copy";
		args = o.args;
		codePath = new ArrayList<String>(o.codePath);
		cookie = o.cookie;
		managed = o.managed;
		homeDir = o.homeDir;
		workingDir = o.workingDir;
		nodeName = o.nodeName + "_copy";
	}

	public void store(Preferences root) {
		Preferences node = root.node(getName());
		String code = PreferencesUtils.packList(getCodePath());
		node.put(CODE_PATH, code);
		node.put(HOME_DIR, getOtpHome());
		node.put(NODENAME, getNodeName());
		node.put(COOKIE, cookie);
		node.put(ARGS, args);
		node.put(WORKING_DIR, workingDir);
		node.put(MANAGED, Boolean.toString(managed));
		node.put(ERLIDE, Boolean.toString(erlide));
	}

	public void load(Preferences node) {
		setName(node.name());
		String path = node.get(CODE_PATH, "");
		setCodePath(PreferencesUtils.unpackList(path));
		setOtpHome(node.get(HOME_DIR, ""));
		nodeName = node.get(NODENAME, "");
		cookie = node.get(COOKIE, "");
		args = node.get(ARGS, "");
		workingDir = node.get(WORKING_DIR, "");
		managed = node.getBoolean(MANAGED, false);
		erlide = node.getBoolean(ERLIDE, false);
	}

	public String getArgs() {
		return this.args;
	}

	public void setArgs(String args) {
		this.args = args;
	}

	public String getCookie() {
		if (cookie == null || cookie.length() == 0) {
			cookie = Cookie.retrieveCookie();
		}
		return this.cookie;
	}

	public void setCookie(String cookie) {
		this.cookie = cookie;
	}

	public String getNodeName() {
		return this.nodeName;
	}

	public void setNodeName(String nodeName) {
		this.nodeName = nodeName;
	}

	public boolean isManaged() {
		return this.managed;
	}

	public void setManaged(boolean managed) {
		this.managed = managed;
	}

	public boolean isErlide() {
		return this.erlide;
	}

	public void setErlide(boolean erlide) {
		this.erlide = erlide;
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

	public void setWorkingDir(String workingDir) {
		this.workingDir = workingDir;
	}

	@Override
	public String toString() {
		return String.format("Runtime<%s (%s)>", getName(), getOtpHome());
	}

	public String getCmdLine() {
		String pathA = cvt(getPathA());
		String pathZ = cvt(getPathZ());
		String cmd = String.format("%s/bin/erl %s %s %s", getOtpHome(),
				ifNotEmpty(" -pa ", pathA), ifNotEmpty(" -pz ", pathZ),
				getArgs());
		cmd += "-name " + BackendManager.buildNodeName(getNodeName())
				+ " -setcookie " + getCookie();
		return cmd;
	}

	private String ifNotEmpty(String key, String str) {
		if (str == null || str.length() == 0) {
			return "";
		}
		return key + str;
	}

	public String getVersion() {
		if (fVersion == null) {
			fVersion = getRuntimeVersion(homeDir);
		}
		return fVersion;
	}

	public String getOtpHome() {
		return homeDir;
	}

	public void setOtpHome(String otpHome) {
		homeDir = otpHome;
		getVersion();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<String> getCodePath() {
		return codePath;
	}

	public void setCodePath(List<String> path) {
		codePath = path;
	}

	protected List<String> getPathA(String marker) {
		if (codePath != null) {
			List<String> list = codePath;
			int i = list.indexOf(marker);
			if (i < 0) {
				return list;
			}
			return list.subList(0, i);
		}
		return Collections.emptyList();
	}

	protected List<String> getPathZ(String marker) {
		if (codePath != null) {
			List<String> list = codePath;
			int i = list.indexOf(marker);
			if (i < 0) {
				return Collections.emptyList();
			}
			return list.subList(i + 1, codePath.size());
		}
		return Collections.emptyList();
	}

	public static boolean validateLocation(String path) {
		final String v = getRuntimeVersion(path);
		return v != null;
	}

	public static String getRuntimeVersion(String path) {
		if (path == null) {
			return null;
		}
		final File boot = new File(path + "/bin/start.boot");
		try {
			final FileInputStream is = new FileInputStream(boot);
			is.skip(14);
			readstring(is);
			return readstring(is);
		} catch (final IOException e) {
		}
		return null;
	}

	private static String readstring(InputStream is) {
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

	public static boolean isValidOtpHome(String otpHome) {
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

	public static boolean hasCompiler(String otpHome) {
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

	protected static String cvt(Collection<String> path) {
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

}
