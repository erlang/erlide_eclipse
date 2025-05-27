package org.erlide.backend;

import static com.google.common.truth.Truth.assertThat;

import java.util.Enumeration;

import org.eclipse.core.runtime.Platform;
import org.junit.Test;
import org.osgi.framework.Bundle;

public class ErlangPluginsTest {

    @Test
    public void commonIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.common", "/ebin");
    }

    @Test
    public void ideIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.ide", "/ebin");
    }

    @Test
    public void debuggerIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger", "/ebin");
    }

    @Test
    public void debugger25IsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger", "/ebin/25");
    }

    @Test
    public void debugger26IsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger", "/ebin/26");
    }

    @Test
    public void debugger27IsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger", "/ebin/27");
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
