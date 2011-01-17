package org.erlide.core.erlang.internal;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;

public class ErlModelMap {

    static ErlModelMap erlModelMap = null;

    Map<String, WeakReference<IErlModuleInternal>> map;

    public static ErlModelMap getDefault() {
        if (erlModelMap == null) {
            erlModelMap = new ErlModelMap();
        }
        return erlModelMap;
    }

    private ErlModelMap() {
        map = new HashMap<String, WeakReference<IErlModuleInternal>>();
    }

    public void put(final String path, final IErlModuleInternal moduleInternal) {
        map.put(path, new WeakReference<IErlModuleInternal>(moduleInternal));
    }

    public IErlModuleInternal get(final String path) {
        final WeakReference<IErlModuleInternal> reference = map.get(path);
        if (reference == null) {
            return null;
        }
        return reference.get();
    }

    public boolean has(final String path) {
        return map.containsKey(path);
    }

    /**
     * Is this really needed?
     * 
     * @param moduleInternal
     * @return
     */
    public boolean has(final IErlModuleInternal moduleInternal) {
        final WeakReference<IErlModuleInternal> reference = new WeakReference<IErlModuleInternal>(
                moduleInternal);
        return map.containsValue(reference);
    }
}
