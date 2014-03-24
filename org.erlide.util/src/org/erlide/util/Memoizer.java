package org.erlide.util;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;

/**
 * A dynamic proxy wrapper that caches method calls to the original object.
 * <p>
 * More specifically, each method on the interface to be memoized should meet
 * all of the following criteria:
 * <ul>
 * <li>The return values of the method should not change from call to call.</li>
 * <li>The method should not have side effects.</li>
 * <li>The method should not take mutable arguments.</li>
 * </ul>
 */
public class Memoizer implements InvocationHandler {

    @SuppressWarnings("unchecked")
    public static <T> T memoize(final T object) {
        return (T) Proxy.newProxyInstance(object.getClass().getClassLoader(), object
                .getClass().getInterfaces(), new Memoizer(object));
    }

    private final Object object;
    private final Cache<Method, Map<List<?>, Object>> caches;

    private Memoizer(final Object object) {
        this.object = object;
        caches = CacheBuilder.newBuilder().expireAfterAccess(1, TimeUnit.HOURS)
                .maximumSize(250).build();
    }

    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args)
            throws Throwable {

        if (method.getReturnType().equals(Void.TYPE)) {
            // Don't cache void methods
            return invoke(method, args);
        }
        final Map<List<?>, Object> cache = getCache(method);
        final List<?> key = Arrays.asList(args);
        Object value = cache.get(key);

        if (value == null && !cache.containsKey(key)) {
            value = invoke(method, args);
            cache.put(key, value);
        }
        return value;
    }

    private Object invoke(final Method method, final Object[] args) throws Throwable {
        try {
            return method.invoke(object, args);
        } catch (final InvocationTargetException e) {
            throw e.getTargetException();
        }
    }

    private synchronized Map<List<?>, Object> getCache(final Method m) {
        Map<List<?>, Object> cache = caches.getIfPresent(m);
        if (cache == null) {
            cache = Collections.synchronizedMap(new HashMap<List<?>, Object>());
            caches.put(m, cache);
        }
        return cache;
    }
}
