package org.erlide.runtime

import java.util.Collection
import org.erlide.runtime.runtimeinfo.RuntimeInfo
import org.junit.Test

import static org.hamcrest.MatcherAssert.assertThat
import static org.hamcrest.Matchers.is

class RuntimeInfoTest {

    @Test
    def void codePath_Runtime_1() {
        val RuntimeInfo info = new RuntimeInfo("dummy")
        val Collection<String> pa = info.getCodePath()
        assertThat(pa.size(), is(0))
        assertThat(info.isValid(), is(false))
    }

//    @Test
//    def void versionLocator() {
//        val RuntimeInfo info1 = new RuntimeInfo("dummy", "", "", #[])
// TODO create dir that looks like an otp home
//        val RuntimeInfo info2 = new RuntimeInfo("dummy", "", "", #[])
//        assertThat(VersionLocator.locateVersion(new RuntimeVersion(18), #[info1, info2], false), is(#[info1]))
//    }
}
