package org.erlide.engine

import io.typefox.lsapi.impl.ClientCapabilitiesImpl
import java.io.File
import java.lang.management.ManagementFactory
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.Status
import org.erlide.util.ErlLogger
import org.erlide.util.services.ExtensionUtils
import org.osgi.framework.BundleActivator
import org.osgi.framework.BundleContext

class ModelApiActivator implements BundleActivator {

    static private IErlangEngine engine

    override void start(BundleContext context) throws Exception {
        ErlLogger.debug("Starting Erlang model api")
        engine = ExtensionUtils.getSingletonExtension("org.erlide.model.api.erlangEngine", IErlangEngine)
        if (engine === null) {
            ErlLogger.warn("Could not instantiate Erlang engine!")
            val Status status = new Status(IStatus.ERROR, "org.erlide.model", "Could not instantiate Erlang engine")
            throw new CoreException(status)
        }
        val modelPlugin = Platform.getBundle("org.erlide.model")
        val params = new ErlangInitializeParamsImpl => [
            stateDir = Platform.getStateLocation(modelPlugin).toPortableString()
            processId = getProcessId(0)
            rootPath = ResourcesPlugin.workspace.root.location.toPortableString
            clientName = "erlide-eclipse"
            capabilities = new ClientCapabilitiesImpl
        ]
        engine.initialize(params);
        ErlLogger.debug("Started model api")
    }

    override void stop(BundleContext context) throws Exception {
        engine.shutdown
        engine = null
    }

    def static IErlangEngine getErlangServer() {
        return engine
    }

    def static void cleanupStateDir() {
        if (engine === null) {
            return;
        }
        val String ndir = engine.getStateDir()
        val File fdir = new File(ndir)
        for (File f : fdir.listFiles()) {
            if (f.isFile()) {
                f.delete()
            }
        }
    }

    def private static int getProcessId(int fallback) {
        // Note: may fail in some JVM implementations
        // therefore fallback has to be provided
        // something like '<pid>@<hostname>', at least in SUN / Oracle JVMs
        val jvmName = ManagementFactory.getRuntimeMXBean().getName()
        val index = jvmName.indexOf('@')

        if (index < 1) {
            // part before '@' empty (index = 0) / '@' not found (index = -1)
            return fallback
        }

        try {
            return Integer.parseInt(jvmName.substring(0, index))
        } catch (NumberFormatException e) {
            // ignore
        }
        return fallback
    }

}
