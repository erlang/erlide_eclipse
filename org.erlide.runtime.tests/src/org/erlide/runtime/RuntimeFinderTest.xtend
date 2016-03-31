package org.erlide.runtime

import org.erlide.runtime.runtimeinfo.RuntimeFinder
import org.junit.Test

import static org.hamcrest.MatcherAssert.assertThat
import static org.hamcrest.Matchers.greaterThan
import static org.hamcrest.Matchers.is
import org.erlide.util.SystemConfiguration

class RuntimeFinderTest {
    @Test
    def void findKerlRuntimes() {
        if(SystemConfiguration.instance.onWindows) return

        val kerl = RuntimeFinder.kerlLocations
        assertThat(kerl.length, is(greaterThan(0)))
    }
}
