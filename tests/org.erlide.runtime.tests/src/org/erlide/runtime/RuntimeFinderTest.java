package org.erlide.runtime;

import org.eclipse.xtext.xbase.lib.Conversions;
import org.erlide.runtime.runtimeinfo.RuntimeFinder;
import org.erlide.util.SystemConfiguration;
import org.junit.Ignore;
import org.junit.Test;

import com.google.common.truth.Truth;

@SuppressWarnings("all")
public class RuntimeFinderTest {
    @Test
    @Ignore
    public void findKerlRuntimes() {
        final boolean _isOnWindows = SystemConfiguration.getInstance().isOnWindows();
        if (_isOnWindows) {
            return;
        }
        final Iterable<String> kerl = RuntimeFinder.getKerlLocations();
        Truth.assertThat(Integer
                .valueOf(((Object[]) Conversions.unwrapArray(kerl, Object.class)).length))
                .isGreaterThan(Integer.valueOf(0));
    }
}
