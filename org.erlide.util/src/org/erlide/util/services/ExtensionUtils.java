package org.erlide.util.services;

import java.lang.reflect.Array;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;

public final class ExtensionUtils {

    /**
     * Get an executable extension that has only one implementor. We do no hard
     * checks, but take the first one.
     */
    public static <T> T getSingletonExtension(final String id,
            final Class<? extends T> clazz) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(id);
        if (elements.length > 1) {
            ErlLogger
                    .warn("There are multiple implementors of extension %s! Picking one of them...",
                            id);
        }
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element
                        .createExecutableExtension("class");
                if (clazz.isInstance(object)) {
                    return clazz.cast(object);
                }
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }
        return null;
    }

    public static <T> Provider<? extends T> getSingletonProviderExtension(
            final String id, final Class<? extends Provider<? extends T>> clazz) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(id);
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element
                        .createExecutableExtension("provider");
                if (clazz.isInstance(object)) {
                    return clazz.cast(object);
                }
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }
        return null;
    }

    /**
     * Get the underlying class for a type, or null if the type is a variable
     * type.
     * 
     * @param type
     *            the type
     * @return the underlying class
     */
    @SuppressWarnings("rawtypes")
    public static Class<?> getClass(final Type type) {
        if (type instanceof Class) {
            return (Class) type;
        } else if (type instanceof ParameterizedType) {
            return getClass(((ParameterizedType) type).getRawType());
        } else if (type instanceof GenericArrayType) {
            final Type componentType = ((GenericArrayType) type)
                    .getGenericComponentType();
            final Class<?> componentClass = getClass(componentType);
            if (componentClass != null) {
                return Array.newInstance(componentClass, 0).getClass();
            }
            return null;
        } else {
            return null;
        }
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
                ErlLogger.error(e);
            }
        }
        return result;
    }

    private ExtensionUtils() {
    }

    public static <T> List<T> getExtensions2(final String id,
            final String name, final Class<T> clazz) {
        final List<T> result = Lists.newArrayList();
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg
                .getConfigurationElementsFor(id);
        for (final IConfigurationElement element : elements) {
            for (final IConfigurationElement child : element.getChildren()) {
                try {
                    final Object object = child
                            .createExecutableExtension("class");
                    if (clazz.isInstance(object)) {
                        result.add(clazz.cast(object));
                    }
                } catch (final CoreException e) {
                    ErlLogger.error(e);
                }
            }
        }
        return result;
    }
}
