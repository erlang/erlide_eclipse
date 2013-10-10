package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.stringContainsInOrder;

import org.erlide.core.executor.ToolExecutor;
import org.erlide.util.SystemConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

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
        final String cmd = onWindows ? "where" : "which";
        final String res = ex.getToolLocation(cmd);
        assertThat(res, stringContainsInOrder(Lists.newArrayList(cmd)));
    }

}
