/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;

public class ErtsLaunchConfiguration {

	private ILaunchConfiguration fConfig;

	public ErtsLaunchConfiguration(ILaunchConfiguration conf) {
		fConfig = conf;
	}

	public String getExtraErtsArgs() {
		String result = "";
		try {
			result = fConfig.getAttribute(
					IErlangLaunchConfigurationAttributes.ERTS_EXTRA_ARGS,
					IErlangLaunchConfigurationAttributes.DEFAULT_EXTRA_ARGS);
		} catch (final CoreException e) {
		}
		return result;
	}

	public String getOtpHome() {
		String result = "";
		try {
			result = fConfig.getAttribute(
					IErlangLaunchConfigurationAttributes.ERTS_OTP_HOME,
					IErlangLaunchConfigurationAttributes.DEFAULT_OTP_HOME);
		} catch (final CoreException e) {
		}
		return result;
	}

	public String getPathA() {
		String result = "";
		try {
			result = fConfig.getAttribute(
					IErlangLaunchConfigurationAttributes.ERTS_PATH_A,
					IErlangLaunchConfigurationAttributes.DEFAULT_PATH_A);
		} catch (final CoreException e) {
		}
		return result;
	}

	public String getPathZ() {
		String result = "";
		try {
			result = fConfig.getAttribute(
					IErlangLaunchConfigurationAttributes.ERTS_PATH_Z,
					IErlangLaunchConfigurationAttributes.DEFAULT_PATH_Z);
		} catch (final CoreException e) {
		}
		return result;
	}

	public String buildCommandLine() {
		return getOtpHome() + File.separator + "bin" + File.separator + "erl"
				+ ifNotEmpty(" -pa ", getPathA())
				+ ifNotEmpty(" -pz ", getPathZ()) + " " + getExtraErtsArgs();
	}

	public String getWorkingDirectory() {
		String result = "";
		try {
			result = fConfig.getAttribute(
					IErlangLaunchConfigurationAttributes.ERTS_WORKING_DIR,
					IErlangLaunchConfigurationAttributes.DEFAULT_WORKING_DIR);
		} catch (final CoreException e) {
		}
		return result;
	}

	private String ifNotEmpty(String key, String str) {
		if (str == null || str.length() == 0) {
			return "";
		}
		return key + str;
	}

}
