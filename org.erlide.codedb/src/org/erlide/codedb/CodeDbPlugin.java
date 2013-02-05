package org.erlide.codedb;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.IRuntimeProvider;
import org.osgi.framework.BundleContext;

import com.ericsson.otp.erlang.RuntimeVersion;

public class CodeDbPlugin extends Plugin {

    public static final String PLUGIN_ID = "org.erlide.codedb";
    private static final String CORE_PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = CORE_PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = CORE_PLUGIN_ID + ".erlbuilder";

    private static BundleContext context;
    static CodeDbPlugin plugin;

    public CodeDbPlugin() {
        super();
        plugin = this;
    }

    public static CodeDbPlugin getDefault() {
        if (plugin == null) {
            plugin = new CodeDbPlugin();
        }
        return plugin;
    }

    static BundleContext getContext() {
        return context;
    }

    @Override
    public void start(final BundleContext bundleContext) throws Exception {
        super.start(bundleContext);
        CodeDbPlugin.context = bundleContext;
    }

    @Override
    public void stop(final BundleContext bundleContext) throws Exception {
        CodeDbPlugin.context = null;
        super.stop(bundleContext);
    }

    public IRpcSite getIdeBackend() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor("org.erlide.runtime.backend");
        for (final IConfigurationElement element : elements) {
            try {
                final IRuntimeProvider provider = (IRuntimeProvider) element
                        .createExecutableExtension("class");
                return provider.get();
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public IRpcSite getBackend(final RuntimeVersion version) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor("org.erlide.runtime", "backend");
        for (final IConfigurationElement element : elements) {
            try {
                final IRuntimeProvider provider = (IRuntimeProvider) element
                        .createExecutableExtension("class");
                return provider.get(version);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public IRpcSite getBackend(final String name) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor("org.erlide.runtime", "backend");
        for (final IConfigurationElement element : elements) {
            try {
                final IRuntimeProvider provider = (IRuntimeProvider) element
                        .createExecutableExtension("class");
                return provider.get(name);
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

}
