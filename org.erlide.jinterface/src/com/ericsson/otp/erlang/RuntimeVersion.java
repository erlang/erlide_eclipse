/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package com.ericsson.otp.erlang;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import org.eclipse.core.runtime.Assert;

public final class RuntimeVersion implements Comparable<RuntimeVersion> {

    public static final int UNUSED = -1;

    private static final char[] minorMap = new char[] { 'A', 'B', 'C' };

    private final int major;
    private final int minor;
    private int micro;

    public RuntimeVersion(final RuntimeVersion other) {
        major = other.major;
        minor = other.minor;
        micro = other.micro;
    }

    public RuntimeVersion(final int major, final int minor, final int micro) {
        Assert.isTrue(major >= UNUSED);
        Assert.isTrue(minor >= UNUSED);
        Assert.isTrue(micro >= UNUSED);
        this.major = major;
        this.minor = minor;
        this.micro = micro;
    }

    public RuntimeVersion(final int major, final int minor) {
        this(major, minor, UNUSED);
    }

    public RuntimeVersion(final int major) {
        this(major, UNUSED, UNUSED);
    }

    public RuntimeVersion(final String version) {
        this(version, null);
    }

    public RuntimeVersion(final String version, final String aMicro) {
        if (version == null || version.length() == 0) {
            major = UNUSED;
            minor = UNUSED;
            micro = UNUSED;
            return;
        }
        Assert.isTrue(version.charAt(0) == 'R');
        int i = 1;
        char c;
        do {
            c = version.charAt(i);
            if (c >= '0' && c <= '9') {
                i++;
            }
        } while (c >= '0' && c <= '9' && i < version.length());
        final String substring = version.substring(1, i);
        major = Integer.parseInt(substring);
        if (i < version.length()) {
            c = version.charAt(i);
            minor = Arrays.binarySearch(minorMap, c);
            i++;
            if (major >= 13) {
                if (i < version.length()) {
                    micro = Integer.parseInt(version.substring(i));
                } else {
                    micro = 0;
                }
            } else {
                if (aMicro != null) {
                    micro = Integer.parseInt(aMicro);
                } else {
                    final int n = version.indexOf('-');
                    if (n == -1) {
                        micro = UNUSED;
                    } else {
                        micro = Integer.parseInt(version.substring(n + 1));
                    }
                }
            }
        } else {
            minor = UNUSED;
            micro = UNUSED;
        }
        Assert.isTrue(major >= UNUSED);
        Assert.isTrue(minor >= UNUSED);
        Assert.isTrue(micro >= UNUSED);
    }

    @Override
    public String toString() {
        if (major == UNUSED) {
            return "";
        }
        String result = "R" + Integer.toString(major);
        if (minor != UNUSED) {
            result += minorMap[minor];
            if (micro != UNUSED) {
                String m = Integer.toString(micro);
                if (major < 13) {
                    result += "-" + m;
                } else {
                    if (micro != 0) {
                        if (m.length() == 1) {
                            m = "0" + m;
                        }
                        result += m;
                    }
                }
            }
        }
        return result;
    }

    @Override
    public boolean equals(final Object v) {
        if (!(v instanceof RuntimeVersion)) {
            return false;
        }
        final RuntimeVersion other = (RuntimeVersion) v;
        return compareTo(other) == 0;
    }

    @Override
    public int compareTo(final RuntimeVersion o) {
        if (major == o.major) {
            if (minor == o.minor) {
                if (micro == o.micro) {
                    return 0;
                } else {
                    return micro - o.micro;
                }
            } else {
                return minor - o.minor;
            }
        } else {
            return major - o.major;
        }
    }

    public boolean isDefined() {
        return major != UNUSED;
    }

    public boolean isCompatible(final RuntimeVersion other) {
        return compareTo(other) >= 0;
    }

    public boolean isReleaseCompatible(final RuntimeVersion other) {
        return major == other.major && isCompatible(other);
    }

    public RuntimeVersion asMinor() {
        return new RuntimeVersion(major, minor);
    }

    public RuntimeVersion asMajor() {
        return new RuntimeVersion(major);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }

    public static String getRuntimeVersion(final String path) {
        if (path == null) {
            return null;
        }
        String result = null;
        final File boot = new File(path + "/bin/start.boot");
        try {
            final FileInputStream is = new FileInputStream(boot);
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
                    final String absolutePath = pathname.getAbsolutePath()
                            .toLowerCase();
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

    public static RuntimeVersion getVersion(final String homeDir) {
        final String label = RuntimeVersion.getRuntimeVersion(homeDir);
        final String micro = RuntimeVersion.getMicroRuntimeVersion(homeDir);
        return new RuntimeVersion(label, micro);
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

    public boolean isStable() {
        return minor > 0;
    }

}
