package org.erlide.backend;

import static com.google.common.truth.Truth.assertThat;

import java.util.Enumeration;

import org.eclipse.core.runtime.Platform;
import org.junit.Test;
import org.osgi.framework.Bundle;

public class ErlangPluginsTest {

    @Test
    public void commonIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/common");
    }

    @Test
    public void ideIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/ide");
    }

    @Test
    public void debuggerIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/debugger");
    }

    @Test
    public void debuggerR16IsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/debugger/r16");
    }

    @Test
    public void debugger17IsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/debugger/17");
    }

    @Test
    public void debugger18IsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/debugger/18");
    }

    private void checkBundleForTwoEbinElements(final String pluginId, final String path) {
        final Bundle b = Platform.getBundle(pluginId);
        final Enumeration<String> paths = b.getEntryPaths(path);
        assertThat(paths).isNotNull();
        // we check for at least two elements
        assertThat(paths.hasMoreElements()).isEqualTo(true);
        assertThat(paths.hasMoreElements()).isEqualTo(true);
    }

}
