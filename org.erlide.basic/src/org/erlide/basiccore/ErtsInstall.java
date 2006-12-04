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
package org.erlide.basiccore;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ErtsInstall {

	private String fName;

	private String fId;

	private String fVersion;

	private String fOtpHome;

	private List<String> fPathA;

	private List<String> fPathZ;

	private String fExtraErtsArgs;

	public ErtsInstall() {
		super();
		// TODO create id
		fId = "unique string";
		fPathA = new ArrayList<String>(5);
		fPathZ = new ArrayList<String>(5);
	}

	public String getId() {
		return fId;
	}

	public String getName() {
		return fName;
	}

	public void setName(String name) {
		this.fName = name;
	}

	public String getVersion() {
		return fVersion;
	}

	public void setVersion(String version) {
		this.fVersion = version;
	}

	public String getExtraErtsArgs() {
		return fExtraErtsArgs;
	}

	public void setExtraErtsArgs(String extraErtsArgs) {
		this.fExtraErtsArgs = extraErtsArgs;
	}

	public String getOtpHome() {
		return fOtpHome;
	}

	public void setOtpHome(String otpHome) {
		this.fOtpHome = otpHome;
	}

	public List<String> getPathA() {
		return fPathA;
	}

	public void setPathA(List<String> pathA) {
		this.fPathA = pathA;
	}

	public List<String> getPathZ() {
		return fPathZ;
	}

	public void setPathZ(List<String> pathZ) {
		this.fPathZ = pathZ;
	}

	public void setId(String id) {
		this.fId = id;
	}

	public static String encodeList(List<String> l) {
		final StringBuffer r = new StringBuffer();
		for (final Iterator i = l.iterator(); i.hasNext();) {
			r.append(i.next());
			r.append((char) 0);
		}
		return r.toString();
	}

	public static List<String> decodeList(String s) {
		final String[] p = s.split("" + (char) 0);
		final List<String> l = new ArrayList<String>(p.length);
		for (String element : p) {
			l.add(element);
		}
		return l;
	}

	public static boolean validateLocation(String path) {
		final String v = retrieveVersion(path);
		return v != null;
	}

	public static String retrieveVersion(String path) {
		final File boot = new File(path + File.separator + "bin"
				+ File.separator + "start.boot");
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
}
