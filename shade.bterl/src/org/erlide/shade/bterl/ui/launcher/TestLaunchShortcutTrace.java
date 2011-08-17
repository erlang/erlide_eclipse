package org.erlide.shade.bterl.ui.launcher;

import org.eclipse.core.resources.IContainer;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlElement;

public class TestLaunchShortcutTrace extends TestLaunchShortcut {

    @Override
    protected ILaunchConfiguration getLaunchConfiguration(final Object target) {
        final ILaunchConfigurationWorkingCopy cfg = (ILaunchConfigurationWorkingCopy) super
                .getLaunchConfiguration(target);
        if (cfg != null && target instanceof IErlElement) {
            final IErlElement elem = (IErlElement) target;
            final IErlModule module = getModuleFor(elem);
            final IContainer dir = module.getResource().getParent();
            cfg.setAttribute(TestLaunchAttributes.PROJECT, dir.getProject()
                    .getName());
            cfg.setAttribute(TestLaunchAttributes.WORKDIR, getResolvedPath(dir));
            cfg.setAttribute(TestLaunchAttributes.SUITE, module.getName());
            cfg.setAttribute(TestLaunchAttributes.CASE, getFunctionName(elem));
            cfg.setAttribute(TestLaunchAttributes.MODE, "trace");
        }
        return cfg;
    }

}
