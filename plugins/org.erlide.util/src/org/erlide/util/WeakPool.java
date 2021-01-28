package org.erlide.util;

import java.lang.ref.WeakReference;
import java.util.WeakHashMap;

@SuppressWarnings("all")
public class WeakPool<K extends Object, T extends Object> {
    private final WeakHashMap<K, WeakReference<T>> pool = new WeakHashMap<>();

    public T get(final K key) {
        T _xblockexpression = null;
        {
            final WeakReference<T> ref = this.pool.get(key);
            T _xifexpression = null;
            if (ref != null) {
                _xifexpression = ref.get();
            } else {
                _xifexpression = null;
            }
            _xblockexpression = _xifexpression;
        }
        return _xblockexpression;
    }

    public WeakReference<T> put(final K key, final T object) {
        final WeakReference<T> _weakReference = new WeakReference<>(object);
        return this.pool.put(key, _weakReference);
    }
}
