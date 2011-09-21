package org.erlide.cover.views.model;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Stores information of covered modules
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class ModuleSet {

    private static Map<String, ModuleStats> map;

    static {
        map = new HashMap<String, ModuleStats>();
    }

    public static void add(final ModuleStats module) {
        map.put(module.getLabel(), module);
    }

    public static ModuleStats get(final String name) {
        return map.get(name);
    }

    /**
     * All analysed module names
     * 
     * @return
     */
    public static Set<String> getNames() {
        return map.keySet();
    }

    public static void clear() {
        map.clear();
    }

    public static Iterator<ModuleStats> iterator() {
        return map.values().iterator();
    }

}
