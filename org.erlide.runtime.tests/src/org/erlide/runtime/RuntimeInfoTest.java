package org.erlide.runtime;

import static org.junit.Assert.*;

import java.util.List;

import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.junit.Test;

public class RuntimeInfoTest {

    @Test
    public void codePath_Runtime_1() {
        final RuntimeInfo info = new RuntimeInfo();
        final List<String> pa = info.getCodePath();
        assertTrue(pa.size() == 0);
    }

}
