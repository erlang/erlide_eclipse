package org.erlide.ui.tests;

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
    public void debuggerR15isAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger.otp.r15");
    }

    @Test
    public void debuggerR16isAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger.otp.r16");
    }

    @Test
    public void debugger17isAvailable() {
        checkBundleForTwoEbinElements("org.erlide.kernel.debugger.otp.17");
    }

    private void checkBundleForTwoEbinElements(final String bundleId) {
        final Bundle b = Platform.getBundle(bundleId);
        final Enumeration<String> paths = b.getEntryPaths("/ebin/");
        assertThat(paths, is(not(nullValue())));
        // we check for two elements, one is '.marker'
        assertThat(paths.hasMoreElements(), is(true));
        paths.nextElement();
        assertThat(paths.hasMoreElements(), is(true));
    }

}
