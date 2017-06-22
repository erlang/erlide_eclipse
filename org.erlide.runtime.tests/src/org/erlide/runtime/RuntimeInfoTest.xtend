package org.erlide.runtime

import java.util.Collection
import org.erlide.runtime.runtimeinfo.RuntimeInfo
import org.junit.Test

import static com.google.common.truth.Truth.assertThat

class RuntimeInfoTest {

    @Test
    def void codePath_Runtime_1() {
        val RuntimeInfo info = new RuntimeInfo("dummy")
        val Collection<String> pa = info.getCodePath()
        assertThat(pa.size()).isEqualTo(0)
        assertThat(info.isValid()).isFalse()
    }

//    @Test
//    def void versionLocator() {
//        val RuntimeInfo info1 = new RuntimeInfo("dummy", "", "", #[])
// TODO create dir that looks like an otp home
//        val RuntimeInfo info2 = new RuntimeInfo("dummy", "", "", #[])
//        assertThat(VersionLocator.locateVersion(new RuntimeVersion(18), #[info1, info2], false)).isEqualTo(#[info1]))
//    }
}
