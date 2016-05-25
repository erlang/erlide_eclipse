package org.erlide.runtime.runtimeinfo

import java.util.Collection
import java.util.List

class VersionLocator {
    /**
     * Locate runtimes with this version. If exact matches exist, they are first in the
     * result list. If strict, only this version or newer are returned, otherwise all. A
     * null or empty version returns all runtimes.
     */
    def static Collection<RuntimeInfo> locateVersion(RuntimeVersion vsn, Collection<RuntimeInfo> runtimes,
        boolean strict) {
        val List<RuntimeInfo> result = newArrayList()
        for (info : runtimes.sortWith[a, b | a.version.compareTo(b.version)]) {
            val v = info.getVersion()
            if (v.isReleaseCompatible(vsn)) {
                result.add(info)
            }
        }
        // at the end, first newer versions
        for (info : runtimes) {
            val v = info.getVersion()
            if (v.compareTo(vsn) > 0) {
                result.add(info)
            }
        }
        // and if necessary, older versions
        if (!strict) {
            for (info : runtimes) {
                result.add(info)
            }
        }
        result.filter[isValid()].toSet
    }
}
