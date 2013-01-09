package org.erlide.runtime.runtimeinfo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import com.ericsson.otp.erlang.RuntimeVersion;

public class VersionLocator {
    /**
     * Locate runtimes with this version or newer. If exact matches exists, they
     * are first in the result list. A null or empty version returns all
     * runtimes.
     */
    public static List<RuntimeInfo> locateVersion(final String version,
            final Collection<RuntimeInfo> runtimes) {
        final RuntimeVersion vsn = new RuntimeVersion(version, null);
        return locateVersion(vsn, runtimes);
    }

    public static List<RuntimeInfo> locateVersion(final RuntimeVersion vsn,
            final Collection<RuntimeInfo> runtimes) {
        final List<RuntimeInfo> result = new ArrayList<RuntimeInfo>();
        for (final RuntimeInfo info : runtimes) {
            final RuntimeVersion v = info.getVersion();
            if (v.isReleaseCompatible(vsn)) {
                result.add(info);
            }
        }
        Collections.reverse(result);
        // add even newer versions, but at the end
        for (final RuntimeInfo info : runtimes) {
            final RuntimeVersion v = info.getVersion();
            if (!result.contains(info) && v.compareTo(vsn) > 0) {
                result.add(info);
            }
        }
        return result;
    }

}
