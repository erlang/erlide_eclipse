package org.erlide.debug.ui.views;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlModel;
import org.erlide.core.model.root.IErlProject;
import org.erlide.debug.ui.utils.ModuleListContentProvider;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.ErlLaunchAttributes;
import org.erlide.utils.CommonUtils;

public class InterpretedModuleListContentProvider extends
        ModuleListContentProvider {

    @SuppressWarnings("unchecked")
    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        modules = EMPTY;
        if (newInput instanceof ILaunchConfiguration) {
            final ILaunchConfiguration launchConfiguration = (ILaunchConfiguration) newInput;
            try {
                final List<String> interpret = launchConfiguration
                        .getAttribute(
                                ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                                new ArrayList<String>());
                addModules(interpret);
            } catch (final CoreException e) {
                ErlLogger.warn(e);
            }
        }
    }

    public void setModules(final Collection<String> interpret) {
        modules = EMPTY;
        addModules(interpret);
    }

    /**
     * Find modules from string list add to IFile-list
     * 
     * @param interpret
     *            the list of strings from prefs (projectName:fileName;... or
     *            moduleName;...)
     */
    public void addModules(final Collection<String> interpret) {
        final IErlModel model = ErlModelManager.getErlangModel();
        for (final String projectColonModule : interpret) {
            // project:module | module
            final String[] projectModule = projectColonModule.split(":");
            IErlModule module = null;
            if (projectModule.length > 1) {
                final IErlProject project = (IErlProject) model
                        .getChildNamed(projectModule[0]);
                if (project != null) {
                    final String mName = projectModule[1];
                    try {
                        final boolean isErlangFile = CommonUtils
                                .isErlangFileContentFileName(mName);
                        final String s = isErlangFile ? mName : mName + ".erl";
                        module = project.getModule(s);
                    } catch (final ErlModelException e) {
                        ErlLogger.warn(e);
                    }
                }
            } else {
                try {
                    module = model.findModule(projectColonModule);
                } catch (final ErlModelException e) {
                }
            }
            addModule(module);
        }
    }

    public void addModules(final List<IErlModule> modules) {
        for (final IErlModule module : modules) {
            addModule(module);
        }
    }

}
