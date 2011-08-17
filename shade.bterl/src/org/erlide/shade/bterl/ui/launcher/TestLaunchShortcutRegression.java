package org.erlide.shade.bterl.ui.launcher;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;

public class TestLaunchShortcutRegression extends TestLaunchShortcut {

    @Override
    protected ILaunchConfiguration getLaunchConfiguration(final Object target) {
        final ILaunchConfigurationWorkingCopy cfg = (ILaunchConfigurationWorkingCopy) super
                .getLaunchConfiguration(target);
        if (cfg != null && target instanceof IProject) {
            final IProject prj = (IProject) target;
            final IFolder dir = getLinkedDir(prj);
            if (dir != null) {
                cfg.setAttribute(TestLaunchAttributes.WORKDIR,
                        getResolvedPath(dir));
            }
            cfg.setAttribute(TestLaunchAttributes.PROJECT, prj.getName());
            cfg.setAttribute(TestLaunchAttributes.MODE, "regression");
        }
        return cfg;
    }

    private IFolder getLinkedDir(final IProject prj) {
        IFolder dir = null;
        try {
            for (final IResource m : prj.members()) {
                if (m.isLinked() && m.getType() == IResource.FOLDER) {
                    dir = (IFolder) m;
                    break;
                }
            }
        } catch (final CoreException e) {
            e.printStackTrace();
        }
        return dir;
    }

}
