package org.erlide.core.services.builder;

import static com.google.common.truth.Truth.assertThat;

import org.erlide.core.executor.ToolExecutor;
import org.erlide.util.SystemConfiguration;
import org.junit.Test;

public class ToolExecutorTest {

    @Test
    public void whichToolShouldBeFound() {
        final boolean onWindows = SystemConfiguration.getInstance().isOnWindows();
        final String cmd = onWindows ? "where" : "which";
        final String res = ToolExecutor.getToolLocation(cmd);
        assertThat(res).isNotNull();
    }

}
