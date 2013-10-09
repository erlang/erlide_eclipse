package org.erlide.core.services.builder;

import org.erlide.core.executor.ToolExecutor;
import org.erlide.core.executor.ToolExecutor.ToolResults;
import org.erlide.util.SystemConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ToolExecutorTests {

    private ToolExecutor ex;

    @Before
    public void before() {
        ex = new ToolExecutor();
    }

    @After
    public void after() {
        ex = null;
    }

    @Test
    public void test1() {
        final boolean onWindows = SystemConfiguration.getInstance()
                .isOnWindows();
        final String cmd = onWindows ? "c:\\Windows\\System32\\where.exe"
                : "c:\\Windows\\System32\\where.exe";
        final ToolResults res = ex.run("c:\\Windows\\System32\\cmd.exe",
                " /c \"where where\"", null);
        System.out.println("CMD -> " + res);
    }

}
