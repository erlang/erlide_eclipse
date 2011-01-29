package org.erlide.test_support.ui.launcher;

import org.eclipse.core.resources.IContainer;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;

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
