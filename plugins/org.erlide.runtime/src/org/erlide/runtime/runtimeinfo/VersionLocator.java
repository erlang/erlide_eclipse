package org.erlide.runtime.runtimeinfo;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

@SuppressWarnings("all")
public class VersionLocator {
    /**
     * Locate runtimes with this version. If exact matches exist, they are first in the
     * result list. If strict, only this version or newer are returned, otherwise all. A
     * null or empty version returns all runtimes.
     */
    public static Collection<RuntimeInfo> locateVersion(final RuntimeVersion vsn,
            final Collection<RuntimeInfo> runtimes, final boolean strict) {
        Set<RuntimeInfo> _xblockexpression = null;
        {
            final List<RuntimeInfo> result = CollectionLiterals
                    .<RuntimeInfo> newArrayList();
            final Comparator<RuntimeInfo> _function = (final RuntimeInfo a,
                    final RuntimeInfo b) -> {
                return a.getVersion().compareTo(b.getVersion());
            };
            final List<RuntimeInfo> _sortWith = IterableExtensions
                    .<RuntimeInfo> sortWith(runtimes, _function);
            for (final RuntimeInfo info : _sortWith) {
                {
                    final RuntimeVersion v = info.getVersion();
                    final boolean _isReleaseCompatible = v.isReleaseCompatible(vsn);
                    if (_isReleaseCompatible) {
                        result.add(info);
                    }
                }
            }
            for (final RuntimeInfo info_1 : runtimes) {
                {
                    final RuntimeVersion v = info_1.getVersion();
                    final int _compareTo = v.compareTo(vsn);
                    final boolean _greaterThan = _compareTo > 0;
                    if (_greaterThan) {
                        result.add(info_1);
                    }
                }
            }
            if (!strict) {
                for (final RuntimeInfo info_2 : runtimes) {
                    result.add(info_2);
                }
            }
            final Function1<RuntimeInfo, Boolean> _function_1 = (
                    final RuntimeInfo it) -> {
                return Boolean.valueOf(it.isValid());
            };
            _xblockexpression = IterableExtensions.<RuntimeInfo> toSet(
                    IterableExtensions.<RuntimeInfo> filter(result, _function_1));
        }
        return _xblockexpression;
    }
}
