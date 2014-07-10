package org.erlide.util.services;

import java.lang.reflect.Array;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
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
        final IConfigurationElement[] elements = reg.getConfigurationElementsFor(id);
        if (elements.length > 1) {
            ErlLogger.warn("There are multiple implementors of extension %s! "
                    + "Picking one of them...", id);
        }
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element.createExecutableExtension("class");
                if (clazz.isInstance(object)) {
                    return clazz.cast(object);
                }
            } catch (final CoreException e) {
                e.printStackTrace();
                // for some reason, ErlLogger only is printed if the above is
                // here...
                ErlLogger.error(e);
            }
        }
        return null;
    }

    public static <T> Provider<? extends T> getSingletonProviderExtension(
            final String id, final Class<? extends Provider<? extends T>> clazz) {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg.getConfigurationElementsFor(id);
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element.createExecutableExtension("provider");
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

    public static <T> List<T> getExtensions(final String id, final Class<T> clazz) {
        final List<T> result = Lists.newArrayList();
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        final IConfigurationElement[] elements = reg.getConfigurationElementsFor(id);
        for (final IConfigurationElement element : elements) {
            try {
                final Object object = element.createExecutableExtension("class");
                if (clazz.isInstance(object)) {
                    result.add(clazz.cast(object));
                }
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }
        return result;
    }

    /**
     * This should be used to add participants at test-time. It should be the
     * name of the extension point to a list (which will be returned)
     */
    public static Map<String, List<Object>> testingParticipants;

    public static IExtension[] getExtensions(final String type) {
        IExtension[] extensions;
        final IExtensionRegistry registry = Platform.getExtensionRegistry();
        // we may not be in eclipse env when testing
        if (registry != null) {
            try {
                final IExtensionPoint extensionPoint = registry.getExtensionPoint(type);
                extensions = extensionPoint.getExtensions();
            } catch (final Exception e) {
                ErlLogger.error("Error getting extension for:" + type + " -- "
                        + e.getMessage());
                throw new RuntimeException(e);
            }
        } else {
            extensions = new IExtension[0];
        }
        return extensions;
    }

    @SuppressWarnings("unchecked")
    public static Object getParticipant(final String type) {
        // only one participant may be used for this
        final List<Object> participants = (List<Object>) getParticipants(type);
        if (participants.size() == 1) {
            return participants.get(0);
        }
        if (participants.isEmpty()) {
            return null;
        }
        if (participants.size() > 1) {
            throw new RuntimeException(
                    "More than one participant is registered for type:" + type);
        }
        throw new RuntimeException("Should never get here!");
    }

    /**
     * @param type
     *            the extension we want to get
     * @return a list of classes created from those extensions
     */
    public static List<?> getParticipants(final String type) {
        if (testingParticipants != null) {
            return testingParticipants.get(type);
        }

        final List<Object> list = Lists.newArrayList();
        final IExtension[] extensions = getExtensions(type);
        // For each extension ...
        for (int i = 0; i < extensions.length; i++) {
            final IExtension extension = extensions[i];
            final IConfigurationElement[] elements = extension.getConfigurationElements();
            // For each member of the extension ...
            for (int j = 0; j < elements.length; j++) {
                final IConfigurationElement element = elements[j];

                try {
                    list.add(element.createExecutableExtension("class"));
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            }
        }
        return list;
    }

    private ExtensionUtils() {
    }

}
