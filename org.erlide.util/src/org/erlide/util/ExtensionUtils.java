package org.erlide.util;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.RegistryFactory;

import com.google.common.collect.Lists;

public class ExtensionUtils {

    /**
     * Get an executable extension that has only one implementor. We do no hard
     * checks, but take the first one.
     */
    public static <T> T getSingletonExtension(final String id,
            final Class<T> clazz) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(id);
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element
                        .createExecutableExtension("class");
                if (clazz.isInstance(object)) {
                    return clazz.cast(object);
                }
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    public static <T> List<T> getExtensions(final String id,
            final Class<T> clazz) {
        final List<T> result = Lists.newArrayList();
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(id);
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element
                        .createExecutableExtension("class");
                if (clazz.isInstance(object)) {
                    result.add(clazz.cast(object));
                }
            } catch (final CoreException e) {
                e.printStackTrace();
            }
        }
        return result;
    }

    private ExtensionUtils() {
    }
}
