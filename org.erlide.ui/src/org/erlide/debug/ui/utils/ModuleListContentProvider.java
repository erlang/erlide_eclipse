package org.erlide.debug.ui.utils;

import java.util.List;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.erlide.engine.model.erlang.IErlModule;

import com.google.common.collect.Lists;

public abstract class ModuleListContentProvider implements IStructuredContentProvider {

    protected static final List<IErlModule> EMPTY = Lists.newArrayList();

    protected List<IErlModule> modules = EMPTY;

    @Override
    public void dispose() {
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return modules.toArray();
    }

    public void addModule(final IErlModule module) {
        if (module != null && !modules.contains(module)) {
            modules.add(module);
        }
    }

    public void removeModule(final IErlModule module) {
        modules.remove(module);
    }

}
