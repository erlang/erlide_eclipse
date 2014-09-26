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
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

import org.eclipse.jdt.annotation.NonNull;

import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.io.Files;

public final class RuntimeInfo {

    private final String name;
    private final String otpHomeDir;
    private final String args;
    private final Collection<String> codePath;
    private final boolean valid;

    private RuntimeVersion version = null;

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

    public RuntimeInfo(final String name, final String otpHomeDir, final String args,
            final Collection<String> codePath) {
        Preconditions.checkArgument(name != null);
        Preconditions.checkArgument(otpHomeDir != null);
        Preconditions.checkArgument(args != null);
        Preconditions.checkArgument(codePath != null);
        this.name = name;
        this.otpHomeDir = otpHomeDir;
        this.args = args;
        this.codePath = ImmutableList.copyOf(codePath);
        valid = isValidOtpHome(otpHomeDir);
    }

    public RuntimeInfo(@NonNull final RuntimeInfo o) {
        this(o.name, o.otpHomeDir, o.args, o.codePath);
    }

    public String getArgs() {
        return args;
    }

    @Override
    public String toString() {
        return String.format("Runtime<%s (%s) %s [%s]>", getName(), getOtpHome(),
                getVersion(), getArgs());
    }

    public String getOtpHome() {
        return otpHomeDir;
    }

    public String getName() {
        return name;
    }

    public Collection<String> getCodePath() {
        return codePath;
    }

    @Override
    public boolean equals(final Object other) {
        if (!(other instanceof RuntimeInfo)) {
            return false;
        }
        final RuntimeInfo other1 = (RuntimeInfo) other;
        return Objects.equal(otpHomeDir + "|" + args + "|" + codePath.toString(),
                other1.otpHomeDir + "|" + other1.args + "|" + other1.codePath.toString());
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(otpHomeDir, args, codePath);
    }

    public boolean isValid() {
        return valid;
    }

    public static boolean validateLocation(final String path) {
        final String v = getRuntimeVersion(path);
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
        if (!isValidOtpHome(otpHome)) {
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
        if (version == null) {
            version = getVersion(otpHomeDir);
        }
        return version;
    }

    public static RuntimeVersion getVersion(final String homeDir) {
        final String label = getRuntimeVersion(homeDir);
        final String micro = getMicroRuntimeVersion(homeDir);
        return RuntimeVersion.Serializer.parse(label, micro);
    }

    public static String getRuntimeVersion(final String path) {
        if (path == null) {
            return null;
        }
        String result = readOtpVersion(path);
        if (result != null) {
            return result;
        }
        result = readReleaseOtpVersion(path + "/releases/");
        if (result != null) {
            return result;
        }
        result = readStartBoot(path);
        return result;
    }

    private static String readOtpVersion(final String path) {
        final File file = new File(path + "/OTP_VERSION");
        try {
            return Files.toString(file, Charsets.US_ASCII).trim();
        } catch (final IOException e) {
        }
        return null;
    }

    private static String readReleaseOtpVersion(final String path) {
        final File dir = new File(path);
        final String[] rels = dir.list();
        if (rels == null) {
            return null;
        }
        // sort!
        for (final String rel : rels) {
            final String result = readOtpVersion(path + rel);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    private static String readStartBoot(final String path) {
        String result = null;
        final File file = new File(path + "/bin/start.boot");
        try {
            final FileInputStream is = new FileInputStream(file);
            try {
                is.skip(14);
                readstring(is);
                result = readstring(is);
            } finally {
                is.close();
            }
        } catch (final IOException e) {
        }
        return result;
    }

    public static String getMicroRuntimeVersion(final String path) {
        if (path == null) {
            return null;
        }
        String result = null;

        // now get micro version from kernel's minor version
        final File lib = new File(path + "/lib");
        final File[] kernels = lib.listFiles(new FileFilter() {
            @Override
            public boolean accept(final File pathname) {
                try {
                    boolean r = pathname.isDirectory();
                    r &= pathname.getName().startsWith("kernel-");
                    final String canonicalPath = pathname.getCanonicalPath()
                            .toLowerCase();
                    final String absolutePath = pathname.getAbsolutePath().toLowerCase();
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
            result = Integer.toString(krnls[krnls.length - 1]);
        }
        return result;
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

}
