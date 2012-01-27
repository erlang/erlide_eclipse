package org.erlide.ui.internal.folding;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.erlide.ui.editors.erl.folding.IErlangFoldingStructureProvider;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.prefs.PreferenceConstants;

public class ErlangFoldingStructureProviderRegistry {

    private static final String EXTENSION_POINT = "foldingStructureProviders"; //$NON-NLS-1$

    /** The map of descriptors, indexed by their identifiers. */
    private Map<String, ErlangFoldingStructureProviderDescriptor> fDescriptors;

    /**
     * Creates a new instance.
     */
    public ErlangFoldingStructureProviderRegistry() {
    }

    /**
     * Returns an array of <code>ErlangFoldingStructureProviderDescriptor</code>
     * describing all extension to the <code>foldingProviders</code> extension
     * point.
     * 
     * @return the list of extensions to the
     *         <code>quickDiffReferenceProvider</code> extension point.
     */
    public ErlangFoldingStructureProviderDescriptor[] getFoldingProviderDescriptors() {
        synchronized (this) {
            ensureRegistered();
            return fDescriptors.values().toArray(
                    new ErlangFoldingStructureProviderDescriptor[fDescriptors
                            .size()]);
        }
    }

    /**
     * Returns the folding provider descriptor with identifier <code>id</code>
     * or <code>null</code> if no such provider is registered.
     * 
     * @param id
     *            the identifier for which a provider is wanted
     * @return the corresponding provider descriptor, or <code>null</code> if
     *         none can be found
     */
    public ErlangFoldingStructureProviderDescriptor getFoldingProviderDescriptor(
            final String id) {
        synchronized (this) {
            ensureRegistered();
            return fDescriptors.get(id);
        }
    }

    /**
     * Instantiates and returns the provider that is currently configured in the
     * preferences.
     * 
     * @return the current provider according to the preferences
     */
    public IErlangFoldingStructureProvider getCurrentFoldingProvider() {
        final String id = ErlideUIPlugin.getDefault().getPreferenceStore()
                .getString(PreferenceConstants.EDITOR_FOLDING_PROVIDER);
        final ErlangFoldingStructureProviderDescriptor desc = getFoldingProviderDescriptor(id);
        if (desc != null) {
            try {
                return desc.createProvider();
            } catch (final CoreException e) {
                ErlideUIPlugin.log(e);
            }
        }
        return null;
    }

    /**
     * Ensures that the extensions are read and stored in
     * <code>fDescriptors</code>.
     */
    private void ensureRegistered() {
        if (fDescriptors == null) {
            reloadExtensions();
        }
    }

    /**
     * Reads all extensions.
     * <p>
     * This method can be called more than once in order to reload from a
     * changed extension registry.
     * </p>
     */
    public void reloadExtensions() {
        final IExtensionRegistry registry = Platform.getExtensionRegistry();
        final Map<String, ErlangFoldingStructureProviderDescriptor> map = new HashMap<String, ErlangFoldingStructureProviderDescriptor>();

        final IConfigurationElement[] elements = registry
                .getConfigurationElementsFor(ErlideUIPlugin.PLUGIN_ID,
                        EXTENSION_POINT);
        for (final IConfigurationElement element : elements) {
            final ErlangFoldingStructureProviderDescriptor desc = new ErlangFoldingStructureProviderDescriptor(
                    element);
            map.put(desc.getId(), desc);
        }

        synchronized (this) {
            fDescriptors = Collections.unmodifiableMap(map);
        }
    }

}
