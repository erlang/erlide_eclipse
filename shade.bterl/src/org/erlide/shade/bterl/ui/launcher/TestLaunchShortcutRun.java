package org.erlide.shade.bterl.ui.launcher;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.ui.IEditorPart;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;

public class TestLaunchShortcutRun extends TestLaunchShortcut {

    @Override
    protected ILaunchConfiguration getLaunchConfiguration(Object target) {
        final ILaunchConfigurationWorkingCopy cfg = (ILaunchConfigurationWorkingCopy) super
                .getLaunchConfiguration(target);
        if (cfg != null) {
            if (target instanceof IEditorPart) {
                target = getEditorTarget(target);
            }
            if (target instanceof IFolder) {
                final IFolder res = (IFolder) target;
                cfg.setAttribute(TestLaunchAttributes.PROJECT, res.getProject()
                        .getName());
                cfg.setAttribute(TestLaunchAttributes.WORKDIR,
                        getResolvedPath(res));
            } else if (target instanceof IResource) {
                final IResource res = (IResource) target;
                cfg.setAttribute(TestLaunchAttributes.PROJECT, res.getProject()
                        .getName());
                cfg.setAttribute(TestLaunchAttributes.WORKDIR,
                        getResolvedPath(res.getParent()));
                cfg.setAttribute(TestLaunchAttributes.SUITE, res.getName());
            } else if (target instanceof IErlElement) {
                final IErlElement elem = (IErlElement) target;
                final IErlModule module = getModuleFor(elem);
                final IContainer dir = module.getResource().getParent();
                cfg.setAttribute(TestLaunchAttributes.PROJECT, dir.getProject()
                        .getName());
                cfg.setAttribute(TestLaunchAttributes.WORKDIR,
                        getResolvedPath(dir));
                cfg.setAttribute(TestLaunchAttributes.SUITE,
                        module.getModuleName());
                cfg.setAttribute(TestLaunchAttributes.CASE,
                        getFunctionName(elem));
            }
            cfg.setAttribute(TestLaunchAttributes.MODE, "run");
        }
        return cfg;
    }

}
