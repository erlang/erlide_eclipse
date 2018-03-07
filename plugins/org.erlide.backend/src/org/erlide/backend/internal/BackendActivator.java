package org.erlide.backend.internal;

import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackendFactory;
import org.erlide.backend.api.IBackendManager;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.util.DebugStream;
import org.erlide.util.ErlLogger;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class BackendActivator implements BundleActivator {

    public static final String PLUGIN_ID = "org.erlide.backend";

    @Override
    public void start(final BundleContext context) throws Exception {
        DebugStream.activate();
        ErlLogger.debug("Backend plugin starting");

        final IRuntimeInfoCatalog catalog = BackendCore.getRuntimeInfoCatalog();
        final IBackendFactory backendFactory = new BackendFactory(catalog);
        final IBackendManager backendManager = new BackendManager(backendFactory);
        BackendCore.init(backendManager);
        ErlLogger.debug("Backend plugin started");
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        BackendCore.stop();
    }
}
