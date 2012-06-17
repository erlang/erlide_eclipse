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
package org.erlide.backend.runtimeinfo;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.ericsson.otp.erlang.RuntimeVersion;

public class RuntimeInfo {
    public static final String DEFAULT_MARKER = "*DEFAULT*";

    private String name;
    private String homeDir = "";
    private String args = "";
    private List<String> codePath;
    private RuntimeVersion version;

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
        rt.homeDir = o.homeDir;
        rt.version = o.version;
        return rt;
    }

    public String getArgs() {
        return args;
    }

    public void setArgs(final String args) {
        this.args = args.trim();
    }

    @Override
    public String toString() {
        return String.format("Runtime<%s (%s) %s [%s]>", getName(),
                getOtpHome(), version, getArgs());
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

    public static boolean validateLocation(final String path) {
        final String v = RuntimeVersion.getRuntimeVersion(path);
        return v != null;
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

        final boolean hasErl = hasExecutableFile(otpHome + "/bin/erl");

        final File lib = new File(otpHome + "/lib");
        final boolean hasLib = lib.isDirectory() && lib.exists();

        return hasErl && hasLib;
    }

    private static boolean hasExecutableFile(final String fileName) {
        final File simpleFile = new File(fileName);
        final File exeFile = new File(fileName + ".exe");
        return simpleFile.exists() || exeFile.exists();
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

        final boolean hasErlc = hasExecutableFile(otpHome + "/bin/erlc");
        return hasErlc;
    }

    protected static String cvt(final Collection<String> path) {
        final StringBuilder result = new StringBuilder();
        for (String s : path) {
            if (s.length() > 0) {
                if (s.contains(" ")) {
                    s = "\"" + s + "\"";
                }
                result.append(s).append(';');
            }
        }
        return result.toString();
    }

    public RuntimeVersion getVersion() {
        return version;
    }

}
