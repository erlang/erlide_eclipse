package org.erlide.runtime.runtimeinfo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class VersionLocator {

    /**
     * Locate runtimes with this version. If exact matches exist, they are first
     * in the result list. If strict, only this version or newer are returned,
     * otherwise all. A null or empty version returns all runtimes.
     */
    public static List<RuntimeInfo> locateVersion(final RuntimeVersion vsn,
            final Collection<RuntimeInfo> runtimes, final boolean strict) {
        final List<RuntimeInfo> result = new ArrayList<RuntimeInfo>();
        for (final RuntimeInfo info : runtimes) {
            final RuntimeVersion v = info.getVersion();
            if (v.isReleaseCompatible(vsn)) {
                result.add(info);
            }
        }
        Collections.reverse(result);
        // at the end, first newer versions
        for (final RuntimeInfo info : runtimes) {
            final RuntimeVersion v = info.getVersion();
            if (!result.contains(info) && v.compareTo(vsn) > 0) {
                result.add(info);
            }
        }
        // and if necessary, older versions
        if (!strict) {
            for (final RuntimeInfo info : runtimes) {
                if (!result.contains(info)) {
                    result.add(info);
                }
            }
        }
        return result;
    }

}
