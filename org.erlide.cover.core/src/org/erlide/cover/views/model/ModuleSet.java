package org.erlide.cover.views.model;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ModuleSet {
    
    private static Map<String, ModuleStats> map;
    
    static {
        map = new HashMap<String, ModuleStats>();
    }
    
    public static void add(ModuleStats module) {
        map.put(module.getLabel(), module);
    }
    
    public static ModuleStats get(String name) {
        return map.get(name);
    }
    
    public static Set<String> getNames() {
        return map.keySet();
    }
    
    public static void clear() {
        map.clear();
    }

}
