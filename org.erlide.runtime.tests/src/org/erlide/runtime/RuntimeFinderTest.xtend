package org.erlide.runtime

import org.erlide.runtime.runtimeinfo.RuntimeFinder
import org.junit.Test

import static org.hamcrest.MatcherAssert.assertThat
import static org.hamcrest.Matchers.greaterThan
import static org.hamcrest.Matchers.is

class RuntimeFinderTest {
    @Test
    def void findKerlRuntimes() {
        val kerl = RuntimeFinder.kerlLocations
        assertThat(kerl.length, is(greaterThan(0)))
    }
}