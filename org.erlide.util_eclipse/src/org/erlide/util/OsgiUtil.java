package org.erlide.util;

import org.osgi.framework.Bundle;

public class OsgiUtil {

    public static Bundle findOsgiBundle(final Bundle bundle,
            final String bundleName) {
        final Bundle[] bundles = bundle.getBundleContext().getBundles();
        for (final Bundle b : bundles) {
            if (b.getSymbolicName().equals(bundleName)) {
                return b;
            }
        }
        return null;
    }

}
