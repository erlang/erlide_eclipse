package org.erlide.backend;

import org.erlide.utils.DebugStream;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class BackendActivator implements BundleActivator {

    @Override
    public void start(final BundleContext context) throws Exception {
        DebugStream.activate();
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
    }

}
