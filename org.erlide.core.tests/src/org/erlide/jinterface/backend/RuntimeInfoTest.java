package org.erlide.jinterface.backend;

import static org.junit.Assert.assertTrue;

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

    @Test
    public void codePathA_Runtime_1() {
        final RuntimeInfo info = new RuntimeInfo();
        final List<String> p = info.getCodePath();
        p.add("zzz");
        p.add(0, "aaa");
        assertTrue(p.size() == 3);
        final List<String> pa = info.getPathA();
        assertTrue(pa.size() == 1);
        assertTrue(pa.get(0).equals("aaa"));
    }

    @Test
    public void codePathZ_Runtime_1() {
        final RuntimeInfo info = new RuntimeInfo();
        final List<String> p = info.getCodePath();
        p.add("zzz");
        p.add(0, "aaa");
        assertTrue(p.size() == 3);
        final List<String> pz = info.getPathZ();
        assertTrue(pz.size() == 1);
        assertTrue(pz.get(0).equals("zzz"));
    }
}
