package org.erlide.runtime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Collection;

import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.junit.Test;

public class RuntimeInfoTest {

    @Test
    public void codePath_Runtime_1() {
        final RuntimeInfo info = new RuntimeInfo("dummy");
        final Collection<String> pa = info.getCodePath();
        assertThat(pa.size(), is(0));
    }

}
