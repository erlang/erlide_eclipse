package org.erlide.backend.internal;

import org.erlide.backend.BackendPlugin;
import org.osgi.framework.Bundle;

public class OsgiUtil {

    public static Bundle findOsgiBundle(final String bundleName) {
        final Bundle[] bundles = BackendPlugin.getDefault().getBundleContext()
                .getBundles();
        for (final Bundle b : bundles) {
            if (b.getSymbolicName().equals(bundleName)) {
                return b;
            }
        }
        return null;
    }

}
