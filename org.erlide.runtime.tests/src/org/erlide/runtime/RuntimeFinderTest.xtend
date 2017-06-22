package org.erlide.runtime

import org.erlide.runtime.runtimeinfo.RuntimeFinder
import org.erlide.util.SystemConfiguration
import org.junit.Test

import static com.google.common.truth.Truth.assertThat

class RuntimeFinderTest {
    @Test
    def void findKerlRuntimes() {
        if(SystemConfiguration.instance.onWindows) return

        val kerl = RuntimeFinder.kerlLocations
        assertThat(kerl.length).isGreaterThan(0)
    }
}
