package org.erlide.engine.model.erlang;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class MapUtils {

    public static <K, V> Map<V, List<K>> inverseList(final Map<K, List<V>> map) {
        final Map<V, List<K>> result = new LinkedHashMap<V, List<K>>();
        for (final Map.Entry<K, List<V>> entry : map.entrySet()) {
            for (final V v : entry.getValue()) {
                if (!result.containsKey(v)) {
                    result.put(v, new ArrayList<K>());
                }
                result.get(v).add(entry.getKey());
            }
        }
        return result;
    }

    public static <K, V> Map<V, Set<K>> inverseSet(final Map<K, Set<V>> map) {
        final Map<V, Set<K>> result = new LinkedHashMap<V, Set<K>>();
        for (final Map.Entry<K, Set<V>> entry : map.entrySet()) {
            for (final V v : entry.getValue()) {
                if (!result.containsKey(v)) {
                    result.put(v, new HashSet<K>());
                }
                result.get(v).add(entry.getKey());
            }
        }
        return result;
    }

}
