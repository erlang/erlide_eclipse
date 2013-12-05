package org.erlide.util.services;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.eclipse.xtext.xbase.lib.Pair;

public class ServiceLocator {
    private final static Map<Pair<Class<?>, Object>, Object> services = new ConcurrentHashMap<Pair<Class<?>, Object>, Object>();
    private final static Map<Pair<Class<?>, Object>, Provider<?>> providers = new ConcurrentHashMap<Pair<Class<?>, Object>, Provider<?>>();

    /**
     * Acquire an implementation of a service, identified by id key. If one has
     * not already been instantiated, instantiate the class defined by the
     * Implementor annotation on the interface. If the implementor is an
     * instance of Provider<T>, use it to instantiate the service.
     */
    public static <T> T get(final Class<T> interfaceClass, final Object key) {
        synchronized (interfaceClass) {
            Object service = services
                    .get(new Pair<Class<?>, Object>(interfaceClass, key));
            if (service == null) {
                final Provider<?> provider = providers.get(new Pair<Class<?>, Object>(
                        interfaceClass, key));
                if (provider == null) {
                    try {
                        final Class<?> implementingClass = interfaceClass.getAnnotation(
                                Implementor.class).value();
                        service = implementingClass.newInstance();
                    } catch (final Exception e) {
                        throw new RuntimeException(e);
                    }
                } else {
                    service = provider.get();
                }
                services.put(new Pair<Class<?>, Object>(interfaceClass, key), service);
            }
            return interfaceClass.cast(service);
        }
    }

    @SuppressWarnings("unchecked")
    public static <T> Provider<T> getProvider(final Class<T> interfaceClass,
            final Object key) {
        synchronized (interfaceClass) {
            final Provider<?> provider = providers.get(new Pair<Class<?>, Object>(
                    interfaceClass, key));
            return (Provider<T>) provider;
        }
    }

    /**
     * Acquire an implementation of a service. If one has not already been
     * instantiated, instantiate the class defined by the Implementor annotation
     * on the interface. If the implementor is an instance of Provider<T>, use
     * it to instantiate the service.
     */
    public static <T> T get(final Class<T> interfaceClass) {
        return get(interfaceClass, null);
    }

    /**
     * Set an alternate service implementation. Typically only called in unit
     * tests.
     */
    public static <T, TI extends T> void set(final Class<T> interfaceClass,
            final Object key, final TI implementor) {
        synchronized (interfaceClass) {
            services.put(new Pair<Class<?>, Object>(interfaceClass, key), implementor);
        }
    }

    public static <T> void setProvider(final Class<T> interfaceClass, final Object key,
            final Provider<T> provider) {
        synchronized (interfaceClass) {
            providers.put(new Pair<Class<?>, Object>(interfaceClass, key), provider);
        }
    }

    /**
     * Set an alternate service implementation. Typically only called in unit
     * tests.
     */
    public static <T, TI extends T> void set(final Class<T> interfaceClass,
            final TI implementor) {
        set(interfaceClass, null, implementor);
    }

    public static <T> void setProvider(final Class<T> interfaceClass,
            final Provider<T> provider) {
        setProvider(interfaceClass, null, provider);
    }
}
