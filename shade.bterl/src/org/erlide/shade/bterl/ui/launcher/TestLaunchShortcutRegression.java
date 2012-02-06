package org.erlide.shade.bterl.ui.launcher;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;

public class TestLaunchShortcutRegression extends TestLaunchShortcut {

    @Override
    protected ILaunchConfiguration getLaunchConfiguration(final Object target) {
        final String targetName = getTargetName(target);
        if (targetName == null) {
            return null;
        }
        final ILaunchManager manager = DebugPlugin.getDefault()
                .getLaunchManager();
        final ILaunchConfigurationType type = manager
                .getLaunchConfigurationType("org.erlide.test_support.regression.launchConfigurationType");
        ILaunchConfigurationWorkingCopy cfg;
        try {
            cfg = type.newInstance(null,
                    "internal_" + targetName.replace('/', '_'));
            if (target instanceof IProject) {
                final IProject prj = (IProject) target;
                final IFolder dir = getLinkedDir(prj);
                if (dir != null) {
                    cfg.setAttribute(TestLaunchAttributes.WORKDIR,
                            getResolvedPath(dir));
                }
                cfg.setAttribute(TestLaunchAttributes.PROJECT, prj.getName());
            }
        } catch (final CoreException e) {
            e.printStackTrace();
            cfg = null;
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
