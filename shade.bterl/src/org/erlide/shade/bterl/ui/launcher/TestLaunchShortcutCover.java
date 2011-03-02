package org.erlide.shade.bterl.ui.launcher;

import org.eclipse.core.resources.IFolder;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

public class TestLaunchShortcutCover extends TestLaunchShortcut {

    @Override
    protected ILaunchConfiguration getLaunchConfiguration(final Object target) {
        final ILaunchConfigurationWorkingCopy cfg = (ILaunchConfigurationWorkingCopy) super
                .getLaunchConfiguration(target);
        if (cfg != null && target instanceof IFolder) {
            final IFolder res = (IFolder) target;
            cfg.setAttribute(TestLaunchAttributes.PROJECT, res.getProject()
                    .getName());
            cfg.setAttribute(TestLaunchAttributes.WORKDIR, getResolvedPath(res));
            cfg.setAttribute(TestLaunchAttributes.MODE, "cover");
        }
        return cfg;
    }
}
