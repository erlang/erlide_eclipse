/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others. All rights reserved. This program and
 * the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.runtimeinfo;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Objects;

public class RuntimeVersion implements Comparable<RuntimeVersion> {

    public static final int UNUSED = Integer.MIN_VALUE;
    public static final RuntimeVersion NO_VERSION = new RuntimeVersion(
            RuntimeVersion.UNUSED);

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
        this(major, minor, RuntimeVersion.UNUSED, null);
    }

    public RuntimeVersion(final int major) {
        this(major, RuntimeVersion.UNUSED, RuntimeVersion.UNUSED, null);
    }

    public static class Serializer {

        public static RuntimeVersion parse(final String version) {
            return Serializer.parse(version, null);
        }

        public static RuntimeVersion parseNew(final String version, final String aMicro) {
            final int major;
            int minor;
            int micro;
            String update_level;

            final Pattern p = Pattern.compile("(\\d+)(\\.\\d+)?(\\.\\d+)?([\\._-].+)?");
            final Matcher m = p.matcher(version);
            if (!m.matches()) {
                return null; // throw?
            }

            major = Integer.parseInt(m.group(1));
            minor = Serializer.getValue(m, 2);
            micro = Serializer.getValue(m, 3);
            final String extra = m.group(4);
            update_level = extra;
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
            if (version == null || version.isEmpty()) {
                return RuntimeVersion.NO_VERSION;
            }
            return Serializer.parseNew(version, aMicro);
        }

    }

    @Override
    public String toString() {
        if (major == RuntimeVersion.UNUSED) {
            return "";
        }
        String result = Integer.toString(major);
        if (minor != RuntimeVersion.UNUSED) {
            result += "." + Integer.toString(minor);
        }
        if (micro != RuntimeVersion.UNUSED) {
            result += "." + Integer.toString(micro);
        }
        if (update_level != null) {
            result += update_level;
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
                    if (update_level == o.update_level) {
                        return 0;
                    }
                    if (update_level == null) {
                        return 1;
                    }
                    if (o.update_level == null) {
                        return -1;
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
        return major != RuntimeVersion.UNUSED;
    }

    public boolean isCompatible(final RuntimeVersion other) {
        return major >= other.major;
    }

    public boolean isReleaseCompatible(final RuntimeVersion other) {
        return major == RuntimeVersion.UNUSED || other.major == RuntimeVersion.UNUSED
                || isCompatible(other);
    }

    public RuntimeVersion asMinor() {
        return new RuntimeVersion(major, minor);
    }

    public RuntimeVersion asMajor() {
        return new RuntimeVersion(major);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(major, minor, micro, update_level);
    }

    public boolean isStable() {
        if (update_level == null) {
            return true;
        }
        try {
            // "-rc1" is unstable, but ".15" is stable
            // The first character is a separator
            Integer.parseInt(update_level.substring(1));
            return true;
        } catch (final NumberFormatException e) {
            return false;
        }
    }

    public int getMajor() {
        return major;
    }

    public int getMinor() {
        return minor;
    }

    public int getMicro() {
        return micro;
    }

}
