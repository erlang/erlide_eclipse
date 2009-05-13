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
package org.erlide.backend;

import java.util.Arrays;

import org.erlide.core.util.Assert;

public final class RuntimeVersion implements Comparable<RuntimeVersion> {

	public static final int UNUSED = -1;

	private static final char[] minorMap = new char[] { 'A', 'B', 'C' };

	private final int major;
	private final int minor;
	private final int micro;

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
		if (version == null || version.length() == 0) {
			this.major = UNUSED;
			this.minor = UNUSED;
			this.micro = UNUSED;
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
			if (i < version.length()) {
				Assert.isTrue(version.charAt(i) == '-');
				micro = Integer.parseInt(version.substring(i + 1));
			} else {
				micro = UNUSED;
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
				result += "-" + Integer.toString(micro);
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
}
