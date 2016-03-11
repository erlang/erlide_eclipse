package org.erlide.backend;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.util.Enumeration;

import org.eclipse.core.runtime.Platform;
import org.junit.Test;
import org.osgi.framework.Bundle;

public class ErlangPluginsTest {

    @Test
    public void commonisAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/common");
    }

    @Test
    public void ideisAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel", "/ide");
    }

    @Test
    public void debuggerIsAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger", "/debugger");
    }

    @Test
    public void debuggerR16isAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger.otp", "/debugger/r16");
    }

    @Test
    public void debugger17isAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger.otp", "/debugger/17");
    }

    @Test
    public void debugger18isAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger.otp", "/debugger/18");
    }

    private void checkBundleForTwoEbinElements(final String pluginId, final String path) {
        final Bundle b = Platform.getBundle(pluginId);
        final Enumeration<String> paths = b.getEntryPaths(path);
        assertThat(paths, is(not(nullValue())));
        // we check for at least two elements
        assertThat(paths.hasMoreElements(), is(true));
        assertThat(paths.hasMoreElements(), is(true));
    }

}
