package org.erlide.util;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class OsgiUtil {

    public static Bundle findOsgiBundle(final Bundle bundle,
            final String bundleName) {
        final BundleContext bundleContext = bundle.getBundleContext();
        if (bundleContext == null) {
            return null;
        }
        final Bundle[] bundles = bundleContext.getBundles();
        for (final Bundle b : bundles) {
            if (b.getSymbolicName().equals(bundleName)) {
                return b;
            }
        }
        return null;
    }

}
