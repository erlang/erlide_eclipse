package org.erlide.core;

import java.util.Map;

import com.google.common.collect.Maps;

public class ServicesMap {
    private final Map<Class<? extends IService>, IService> map;

    public ServicesMap() {
        map = Maps.newHashMap();
    }

    public <T extends IService> T getService(Class<T> key) {
        return key.cast(map.get(key));
    }

    public <T extends IService> void putService(Class<T> key, T value) {
        map.put(key, value);
    }

}
