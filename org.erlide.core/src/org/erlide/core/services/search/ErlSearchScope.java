package org.erlide.core.services.search;

import java.util.Collection;
import java.util.Set;

import org.erlide.core.model.erlang.IErlModule;

import com.google.common.collect.Sets;

public class ErlSearchScope {

    private final Set<IErlModule> modules;

    public ErlSearchScope() {
        modules = Sets.newHashSet();
    }

    public void addModule(final IErlModule module) {
        modules.add(module);
    }

    public Collection<IErlModule> getModules() {
        return modules;
    }

    public int size() {
        return modules.size();
    }
}
