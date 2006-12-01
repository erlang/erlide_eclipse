/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/

package org.erlide.basiccore;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.basicui.ErlideBasicUIPlugin;
import org.erlide.basicui.prefs.IPrefConstants;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class ErtsPreferences {

	private List<ErtsInstall> ertsInstallations;

	private ErtsInstall defaultInstall;

	// -----------------

	private Preferences prefs;

	private static final IPreferencesService prefsService = Platform
			.getPreferencesService();

	public ErtsPreferences() {
		prefs = new InstanceScope().getNode(ErlideBasicUIPlugin.PLUGIN_ID
				+ "/erts");
		try {
			prefs.sync();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}

		defaultInstall = new ErtsInstall();
		ertsInstallations = new ArrayList<ErtsInstall>(5);
		ertsInstallations.add(defaultInstall);

		load();
	}

	public void load() {

		defaultInstall.setOtpHome(prefs.get(IPrefConstants.ERTS_OTP_HOME,
				IPrefConstants.DEFAULT_OTP_HOME));
		defaultInstall.setPathA(cvt(prefs.get(IPrefConstants.ERTS_PATH_A,
				IPrefConstants.DEFAULT_PATH_A)));
		defaultInstall.setPathZ(cvt(prefs.get(IPrefConstants.ERTS_PATH_Z,
				IPrefConstants.DEFAULT_PATH_Z)));
		defaultInstall.setExtraErtsArgs(prefs.get(
				IPrefConstants.ERTS_EXTRA_ARGS,
				IPrefConstants.DEFAULT_EXTRA_ARGS));
	}

	private List<String> cvt(String string) {
		final List<String> r = new ArrayList<String>(1);
		r.add(string);
		return r;
	}

	public void store() {
		prefs.put(IPrefConstants.ERTS_OTP_HOME, defaultInstall.getOtpHome());
		prefs.put(IPrefConstants.ERTS_PATH_A, cvt(defaultInstall.getPathA()));
		prefs.put(IPrefConstants.ERTS_PATH_Z, cvt(defaultInstall.getPathZ()));
		prefs.put(IPrefConstants.ERTS_EXTRA_ARGS, defaultInstall
				.getExtraErtsArgs());
		try {
			prefs.flush();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}
	}

	private String cvt(List<String> path) {
		if (path.size() > 0) {
			return path.get(0);
		}
		return "";
	}

	public String getExtraErtsArgs() {
		return defaultInstall.getExtraErtsArgs();
	}

	public void setExtraErtsArgs(String extraErtsArgs) {
		defaultInstall.setExtraErtsArgs(extraErtsArgs.trim());
	}

	public String getOtpHome() {
		return defaultInstall.getOtpHome();
	}

	public void setOtpHome(String otpHome) {
		defaultInstall.setOtpHome(otpHome.trim());
	}

	public String getPathA() {
		return cvt(defaultInstall.getPathA());
	}

	public void setPathA(String pathA) {
		defaultInstall.setPathA(cvt(pathA));
	}

	public String getPathZ() {
		return cvt(defaultInstall.getPathZ());
	}

	public void setPathZ(String pathZ) {
		defaultInstall.setPathZ(cvt(pathZ));
	}

	public String buildCommandLine() {
		return defaultInstall.getOtpHome() + File.separator + "bin"
				+ File.separator + "erl" + ifNotEmpty(" -pa ", getPathA())
				+ ifNotEmpty(" -pz ", getPathZ()) + " "
				+ defaultInstall.getExtraErtsArgs();
	}

	private String ifNotEmpty(String key, String str) {
		if (str == null || str.length() == 0) {
			return "";
		}
		return key + str;
	}

	public String getWorkingDirectory() {
		// TODO get real working directory (= project dir?)
		return ".";
	}

	/**
	 * Updates the underlying plugin preferences to the current state.
	 */
	public void updatePluginPreferences() {
		// prefs.removePropertyChangeListener(this);
		store();
		try {
			prefsService.getRootNode().node(InstanceScope.SCOPE).node(
					ErlideBasicUIPlugin.PLUGIN_ID).flush();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}
		// prefs.addPropertyChangeListener(this);

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

		final File erl = new File(otpHome + File.separator + "bin" + File.separator
				+ "erl");
		final File erlexe = new File(otpHome + File.separator + "bin"
				+ File.separator + "erl.exe");
		final boolean hasErl = erl.exists() || erlexe.exists();

		final File erlc = new File(otpHome + File.separator + "bin" + File.separator
				+ "erlc");
		final File erlcexe = new File(otpHome + File.separator + "bin"
				+ File.separator + "erlc.exe");
		final boolean hasErlc = erlc.exists() || erlcexe.exists();

		final File lib = new File(otpHome + File.separator + "lib");
		final boolean hasLib = lib.isDirectory() && lib.exists();

		return hasErl && hasErlc && hasLib;
	}

}
