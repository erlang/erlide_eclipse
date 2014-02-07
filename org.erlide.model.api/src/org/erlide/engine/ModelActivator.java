package org.erlide.engine;

import java.io.File;

import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class ModelActivator implements BundleActivator {

    private static IErlangEngine engine;

    @Override
    public void start(final BundleContext context) throws Exception {
        ErlLogger.debug("Starting Erlang model api");
        engine = ExtensionUtils.getSingletonExtension(
                "org.erlide.model.api.erlangEngine", IErlangEngine.class);
        if (engine == null) {
            ErlLogger.warn("Could not instantiate Erlang engine!");
        }

        cleanupStateDir();
        ErlLogger.debug("Started model api");
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
    }

    public static IErlangEngine getErlangEngine() {
        return engine;
    }

    private void cleanupStateDir() {
        if (engine == null) {
            return;
        }
        final String ndir = engine.getStateDir();
        final File fdir = new File(ndir);
        for (final File f : fdir.listFiles()) {
            if (f.isFile()) {
                f.delete();
            }
        }
    }
}
