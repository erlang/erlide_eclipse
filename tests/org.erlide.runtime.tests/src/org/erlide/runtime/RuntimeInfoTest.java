package org.erlide.runtime;

import java.util.Collection;

import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.junit.Test;

import com.google.common.truth.Truth;

@SuppressWarnings("all")
public class RuntimeInfoTest {
    @Test
    public void codePath_Runtime_1() {
        final RuntimeInfo info = new RuntimeInfo("dummy");
        final Collection<String> pa = info.getCodePath();
        Truth.assertThat(Integer.valueOf(pa.size())).isEqualTo(Integer.valueOf(0));
        Truth.assertThat(Boolean.valueOf(info.isValid())).isFalse();
    }
}
