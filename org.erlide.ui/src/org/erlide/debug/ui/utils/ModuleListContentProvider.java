package org.erlide.debug.ui.utils;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.launch.ErlLaunchAttributes;

import com.google.common.collect.Lists;

public class ModuleListContentProvider implements IStructuredContentProvider {
    private static final List<IErlModule> EMPTY = Lists.newArrayList();

    final boolean DISABLED = !true;

    List<IErlModule> modules = EMPTY;

    public ModuleListContentProvider() {
        super();
    }

    @SuppressWarnings("unchecked")
    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        try {
            // setRoot(new DebugTreeItem(null, null));
            if (DISABLED) {
                return;
            }
            if (newInput instanceof ILaunchConfiguration) {
                final ILaunchConfiguration launchConfiguration = (ILaunchConfiguration) newInput;
                List<String> interpret;
                try {
                    interpret = launchConfiguration.getAttribute(
                            ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                            new ArrayList<String>());
                } catch (final CoreException e1) {
                    interpret = new ArrayList<String>();
                }
                modules = Lists.newArrayListWithCapacity(interpret.size());
                final IErlModel model = ErlModelManager.getErlangModel();
                for (final String projectColonModule : interpret) {
                    final String[] projectModule = projectColonModule
                            .split(":");
                    final IErlProject project = (IErlProject) model
                            .getChildNamed(projectModule[0]);
                    final IErlModule module = project
                            .getModule(projectModule[1]);
                    modules.add(module);
                }
            } else {
                modules = Lists.newArrayList();
            }
        } catch (final CoreException e1) {
        }
    }

    @Override
    public void dispose() {
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return modules.toArray();
    }

}
