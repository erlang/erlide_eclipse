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
package org.erlide.runtime.runtimeinfo;

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Preconditions;

public class RuntimeVersion implements Comparable<RuntimeVersion> {

    public static final int UNUSED = Integer.MIN_VALUE;
    public static final RuntimeVersion NO_VERSION = new RuntimeVersion(UNUSED);

    private final int major;
    private final int minor;
    private final int micro;
    private final String update_level;

    public RuntimeVersion(final RuntimeVersion other) {
        major = other.major;
        minor = other.minor;
        micro = other.micro;
        update_level = other.update_level;
    }

    public RuntimeVersion(final int major, final int minor, final int micro,
            final String update_level) {
        this.major = major;
        this.minor = minor;
        this.micro = micro;
        this.update_level = update_level;
    }

    public RuntimeVersion(final int major, final int minor, final int micro) {
        this(major, minor, micro, null);
    }

    public RuntimeVersion(final int major, final int minor) {
        this(major, minor, UNUSED, null);
    }

    public RuntimeVersion(final int major) {
        this(major, UNUSED, UNUSED, null);
    }

    public static class Serializer {

        private static final char[] minorMap = new char[] { 'A', 'B', 'C' };

        public static RuntimeVersion parse(final String version) {
            return parse(version, null);
        }

        public static RuntimeVersion parseOld(final String version, final String aMicro) {
            int major;
            int minor = UNUSED;
            int micro = UNUSED;
            String update_level = null;

            Preconditions.checkArgument(version.charAt(0) == 'R');
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
                if (i < version.length()) {
                    final int n = version.indexOf('-');
                    if (n == -1) {
                        micro = Integer.parseInt(version.substring(i));
                    } else {
                        micro = Integer.parseInt(version.substring(i, n));
                        update_level = version.substring(n + 1);
                    }
                } else {
                    micro = 0;
                }
            }
            return new RuntimeVersion(major, minor, micro, update_level);
        }

        public static RuntimeVersion parseNew(final String version, final String aMicro) {
            final int major;
            int minor = 0;
            int micro = 0;
            String update_level = null;

            final Pattern p = Pattern
                    .compile("(\\d+)(\\.\\d+)?(\\.\\d+)?(-[0-9A-Za-z-]+)?");
            final Matcher m = p.matcher(version);
            if (!m.matches()) {
                return null; // throw?
            }

            major = Integer.parseInt(m.group(1));
            minor = getValue(m, 2);
            micro = getValue(m, 3);
            final String extra = m.group(4);
            if (extra != null) {
                try {
                    update_level = extra.substring(1);
                } catch (final Exception e) {
                    update_level = null;
                }
            }
            return new RuntimeVersion(major, minor, micro, update_level);
        }

        private static int getValue(final Matcher m, final int i) {
            final String group = m.group(i);
            if (group == null) {
                return 0;
            }
            return Integer.parseInt(group.substring(1));
        }

        public static RuntimeVersion parse(final String version, final String aMicro) {
            if (version == null || version.length() == 0) {
                return NO_VERSION;
            }

            if (version.charAt(0) == 'R') {
                return parseOld(version, aMicro);
            }
            return parseNew(version, aMicro);
        }

        public static String toStringOld(final RuntimeVersion version) {
            String result = "R" + Integer.toString(version.major);
            if (version.minor != UNUSED) {
                result += minorMap[version.minor];
                if (version.micro != UNUSED) {
                    String m = Integer.toString(version.micro);
                    if (version.micro != 0) {
                        if (m.length() == 1) {
                            m = "0" + m;
                        }
                        result += m;
                        if (version.update_level != null) {
                            result += "-" + version.update_level;
                        }
                    }
                }
            }
            return result;
        }

        public static String toStringNew(final RuntimeVersion version) {
            String result = Integer.toString(version.major);
            if (version.minor != UNUSED) {
                result += "." + Integer.toString(version.minor);
            }
            if (version.micro != UNUSED) {
                result += "." + Integer.toString(version.micro);
            }
            if (version.update_level != null) {
                result += "-" + version.update_level;
            }
            return result;
        }

    }

    @Override
    public String toString() {
        if (major == UNUSED) {
            return "";
        }
        if (major < 17) {
            return Serializer.toStringOld(this);
        }
        return Serializer.toStringNew(this);
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
            final int isNew = major >= 17 ? -1 : 1;
            if (minor == o.minor) {
                if (micro == o.micro) {
                    if (update_level == o.update_level) {
                        return 0;
                    }
                    if (update_level == null) {
                        return -1 * isNew;
                    }
                    if (o.update_level == null) {
                        return 1 * isNew;
                    }
                    return update_level.compareTo(o.update_level);
                }
                return micro - o.micro;
            }
            return minor - o.minor;
        }
        return major - o.major;
    }

    public boolean isDefined() {
        return major != UNUSED;
    }

    public boolean isCompatible(final RuntimeVersion other) {
        return major >= other.major;
    }

    public boolean isReleaseCompatible(final RuntimeVersion other) {
        return major == UNUSED || other.major == UNUSED || isCompatible(other);
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

    public boolean isStable() {
        return minor > 0;
    }

    public int getMajor() {
        return major;
    }

    public int getMinor() {
        return major;
    }

    public int getMicro() {
        return major;
    }

}
