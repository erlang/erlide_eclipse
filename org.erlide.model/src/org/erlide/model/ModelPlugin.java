package org.erlide.model;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.IRuntimeProvider;
import org.osgi.framework.BundleContext;

import com.ericsson.otp.erlang.RuntimeVersion;

public class ModelPlugin extends Plugin {

    public static final String PLUGIN_ID = "org.erlide.model";
    private static final String CORE_PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = CORE_PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = CORE_PLUGIN_ID + ".erlbuilder";

    private static BundleContext context;
    static ModelPlugin plugin;

    public ModelPlugin() {
        super();
        plugin = this;
    }

    public static ModelPlugin getDefault() {
        if (plugin == null) {
            plugin = new ModelPlugin();
        }
        return plugin;
    }

    static BundleContext getContext() {
        return context;
    }

    @Override
    public void start(final BundleContext bundleContext) throws Exception {
        super.start(bundleContext);
        ModelPlugin.context = bundleContext;
    }

    @Override
    public void stop(final BundleContext bundleContext) throws Exception {
        ModelPlugin.context = null;
        super.stop(bundleContext);
    }

    private IRpcSite evalWithProviders(
            final Function1<IRuntimeProvider, IRpcSite> callback) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor("org.erlide.runtime.backend");
        for (final IConfigurationElement element : elements) {
            try {
                final IRuntimeProvider provider = (IRuntimeProvider) element
                        .createExecutableExtension("class");
                return callback.apply(provider);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public IRpcSite getIdeBackend() {
        return evalWithProviders(new Function1<IRuntimeProvider, IRpcSite>() {
            @Override
            public IRpcSite apply(final IRuntimeProvider provider) {
                return provider.get();
            }
        });
    }

    public IRpcSite getBackend(final RuntimeVersion version) {
        return evalWithProviders(new Function1<IRuntimeProvider, IRpcSite>() {
            @Override
            public IRpcSite apply(final IRuntimeProvider provider) {
                return provider.get(version);
            }
        });
    }

    public IRpcSite getBackend(final String name) {
        return evalWithProviders(new Function1<IRuntimeProvider, IRpcSite>() {
            @Override
            public IRpcSite apply(final IRuntimeProvider provider) {
                return provider.get(name);
            }
        });
    }

}
