package org.erlide.jinterface.backend;

import static org.junit.Assert.*;

import java.util.List;

import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.junit.Test;

public class RuntimeInfoTest {

    @Test
    public void codePath_Runtime_1() {
        final RuntimeInfo info = new RuntimeInfo();
        final List<String> pa = info.getCodePath();
        assertTrue(pa.size() == 1);
        assertTrue(pa.get(0).equals(RuntimeInfo.DEFAULT_MARKER));
    }

}
