/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.IOException;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.core.erlang.util.Util;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;


/**
 * 
 * 
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public final class BackendUtil {
	private BackendUtil() {
	}

	public static OtpErlangObject ok(final OtpErlangObject v0) {
		if (!(v0 instanceof OtpErlangTuple)) {
			return v0;
		}
		final OtpErlangTuple v = (OtpErlangTuple) v0;
		if (Util.isOk(v)) {
			return v.elementAt(1);
		}
		return v;
	}

	public static IProject[] getProjects(final String attribute) {
		final String[] projectNames = attribute.split(";");
		return getProjects(projectNames);
	}

	public static IProject[] getProjects(final String[] projectNames) {
		final List<IProject> projects = new ArrayList<IProject>();
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		for (final String s : projectNames) {
			if (s != null && s.length() > 0) {
				final IProject p = root.getProject(s);
				if (p != null) {
					projects.add(p);
				}
			}
		}
		return projects.toArray(new IProject[projects.size()]);
	}

	public static String buildNodeName(final String label,
			final boolean longName) {
		if (label.indexOf('@') > 0) {
			// ignore unique here?
			return label;
		}
		if (longName) {
			final String host = getHost();
			return label + "@" + host;
		} else {
			return label;
		}
	}

	public static String getHost() {
		String host;
		try {
			host = InetAddress.getLocalHost().getHostName();
			if (System.getProperty("erlide.host") != null) {
				final int dot = host.indexOf(".");
				if (dot != -1) {
					host = host.substring(0, dot);
				}
			}
		} catch (final IOException e) {
			host = "localhost";
			ErlLogger.error(e);
		}
		return host;
	}

	public static String getJavaNodeName() {
		final String fUniqueId = BackendUtil.getTimeSuffix();
		return "jerlide_" + fUniqueId;
	}

	static String getErlideNameSuffix() {
		String fUniqueId;
		final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
		final String location = root.getLocation().toPortableString();
		fUniqueId = Long.toHexString(location.hashCode() & 0xFFFFFFF);
		return fUniqueId;
	}

	public static String getLabelProperty() {
		return System.getProperty("erlide.label", null);
	}

	static String getTimeSuffix() {
		String fUniqueId;
		fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFFF);
		return fUniqueId;
	}

}
