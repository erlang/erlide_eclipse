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
package org.erlide.runtime.runtimeinfo;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.eclipse.jdt.annotation.NonNull;

import com.google.common.collect.Lists;

public final class RuntimeInfo {

    private final String name;
    private final String homeDir;
    private final String args;
    private final Collection<String> codePath;

    private RuntimeVersion version_cached = null;

    public static final RuntimeInfo NO_RUNTIME_INFO = new RuntimeInfo("");

    public static class Builder {
        private String name;
        private String homeDir;
        private String args;
        private Collection<String> codePath;

        public Builder() {
            name = "";
            homeDir = ".";
            args = "";
            codePath = Lists.newArrayList();
        }

        public Builder(@NonNull final RuntimeInfo info) {
            name = info.getName();
            homeDir = info.getOtpHome();
            args = info.getArgs();
            codePath = info.getCodePath();
        }

        @NonNull
        public RuntimeInfo build() {
            return new RuntimeInfo(name, homeDir, args, codePath);
        }

        public Builder withName(final String aName) {
            name = aName;
            return this;
        }

        public Builder withHomeDir(final String aHomeDir) {
            homeDir = aHomeDir;
            return this;
        }

        public Builder withArgs(final String someArgs) {
            args = someArgs;
            return this;
        }

        public Builder withCodePath(final Collection<String> aCodePath) {
            codePath = aCodePath;
            return this;
        }
    }

    public RuntimeInfo(final String name) {
        this(name, ".", "", new ArrayList<String>());
    }

    public RuntimeInfo(final String name, final String homeDir,
            final String args, final Collection<String> codePath) {
        this.name = name;
        this.homeDir = homeDir;
        this.args = args;
        this.codePath = Collections.unmodifiableCollection(codePath);
    }

    public RuntimeInfo(@NonNull final RuntimeInfo o) {
        this(o.name, o.homeDir, o.args, o.codePath);
    }

    public String getArgs() {
        return args;
    }

    @Override
    public String toString() {
        return String.format("Runtime<%s (%s) %s [%s]>", getName(),
                getOtpHome(), getVersion(), getArgs());
    }

    public String getOtpHome() {
        return homeDir;
    }

    public String getName() {
        return name;
    }

    public Collection<String> getCodePath() {
        return codePath;
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
        if (version_cached == null) {
            version_cached = RuntimeVersion.getVersion(homeDir);
        }
        return version_cached;
    }

}
