package org.erlide.engine;

import java.io.File;
import java.lang.management.ManagementFactory;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.util.ErlLogger;
import org.erlide.util.services.ExtensionUtils;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Version;

@SuppressWarnings("all")
public class ModelApiActivator implements BundleActivator {
    private static IErlangEngine engine;

    @Override
    public void start(final BundleContext context) throws Exception {
        ErlLogger.debug("Starting Erlang model api");
        ModelApiActivator.engine = ExtensionUtils.<IErlangEngine> getSingletonExtension(
                "org.erlide.model.api.erlangEngine", IErlangEngine.class);
        if (ModelApiActivator.engine == null) {
            ErlLogger.warn("Could not instantiate Erlang engine!");
            final Status status = new Status(IStatus.ERROR, "org.erlide.model",
                    "Could not instantiate Erlang engine");
            throw new CoreException(status);
        }
        final Bundle kernelPlugin = Platform.getBundle("org.erlide.kernel.common");
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append("Kernel: ");
        final Version _version = kernelPlugin.getVersion();
        _builder.append(_version);
        ErlLogger.debug(_builder.toString());
        final Bundle modelPlugin = Platform.getBundle("org.erlide.model");
        final ErlangInitializeParamsImpl _erlangInitializeParamsImpl = new ErlangInitializeParamsImpl();
        final Procedure1<ErlangInitializeParamsImpl> _function = (
                final ErlangInitializeParamsImpl it) -> {
            it.setStateDir(Platform.getStateLocation(modelPlugin).toPortableString());
        };
        final ErlangInitializeParamsImpl params = ObjectExtensions
                .<ErlangInitializeParamsImpl> operator_doubleArrow(
                        _erlangInitializeParamsImpl, _function);
        ModelApiActivator.engine.initialize(params);
        ErlLogger.debug("Started model api");
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        ModelApiActivator.engine.shutdown();
        ModelApiActivator.engine = null;
    }

    public static IErlangEngine getErlangServer() {
        return ModelApiActivator.engine;
    }

    public static void cleanupStateDir() {
        if (ModelApiActivator.engine == null) {
            return;
        }
        final String ndir = ModelApiActivator.engine.getStateDir();
        final File fdir = new File(ndir);
        final File[] _listFiles = fdir.listFiles();
        for (final File f : _listFiles) {
            final boolean _isFile = f.isFile();
            if (_isFile) {
                f.delete();
            }
        }
    }

    private static int getProcessId(final int fallback) {
        final String jvmName = ManagementFactory.getRuntimeMXBean().getName();
        final int index = jvmName.indexOf("@");
        if (index < 1) {
            return fallback;
        }
        try {
            return Integer.parseInt(jvmName.substring(0, index));
        } catch (final Throwable _t) {
            if (_t instanceof NumberFormatException) {
            } else {
                throw Exceptions.sneakyThrow(_t);
            }
        }
        return fallback;
    }
}
