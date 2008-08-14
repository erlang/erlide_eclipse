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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.erlide.basicui.util.PreferencesUtils;
import org.osgi.service.prefs.Preferences;

public class InstallationInfo extends InfoElement {

	private static final String ARGS = "args";
	private static final String HOME_DIR = "homeDir";

	public static final String DEFAULT_MARKER = "*DEFAULT*";

	private String fHomeDir = "";
	private String fArgs = "";

	private String fVersion;
	private List<BackendInfo> backends;

	public InstallationInfo() {
		super();
		backends = new ArrayList<BackendInfo>();
		getCodePath().add(DEFAULT_MARKER);
	}

	public String getVersion() {
		if (fVersion == null) {
			fVersion = getRuntimeVersion(fHomeDir);
		}
		return fVersion;
	}

	public String getArgs() {
		return fArgs;
	}

	public void setArgs(String args) {
		fArgs = args;
	}

	public String getOtpHome() {
		return fHomeDir;
	}

	public void setOtpHome(String otpHome) {
		fHomeDir = otpHome;
		getVersion();
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

	@Override
	public String toString() {
		return String.format("Runtime<%s, \"%s\", '%s', [%s]>", getName(),
				getOtpHome(), getArgs(), PreferencesUtils
						.packList(getCodePath()));
	}

	@Override
	public void store(Preferences root) {
		super.store(root);
		Preferences node = root.node(getName());
		node.put(HOME_DIR, getOtpHome());
		node.put(ARGS, getArgs());
	}

	@Override
	public void load(Preferences node) {
		super.load(node);
		setOtpHome(node.get(HOME_DIR, ""));
		setArgs(node.get(ARGS, ""));
	}

	@Override
	public List<String> getPathA() {
		return getPathA(DEFAULT_MARKER);
	}

	@Override
	public List<String> getPathZ() {
		return getPathZ(DEFAULT_MARKER);
	}

	@Override
	public String getCmdLine() {
		String pathA = cvt(getPathA());
		String pathZ = cvt(getPathZ());
		return String.format("%s/bin/erl %s %s %s", getOtpHome(), ifNotEmpty(
				" -pa ", pathA), ifNotEmpty(" -pz ", pathZ), getArgs());
	}

	private String ifNotEmpty(String key, String str) {
		if (str == null || str.length() == 0) {
			return "";
		}
		return key + str;
	}

	public List<BackendInfo> getBackends() {
		return backends;
	}

}
