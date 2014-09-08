package org.erlide.engine.internal.model.root;

import java.io.File;
import java.io.FilenameFilter;

import org.eclipse.core.resources.ProjectScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.IProjectConfiguratorFactory;
import org.erlide.engine.model.root.OTPProjectConfigurator;
import org.erlide.engine.model.root.ProjectConfigType;

public class ProjectConfiguratorFactory implements IProjectConfiguratorFactory {
    private static IProjectConfiguratorFactory instance;

    public static IProjectConfiguratorFactory getDefault() {
        if (instance == null) {
            instance = new ProjectConfiguratorFactory();
        }
        return instance;
    }

    private ProjectConfiguratorFactory() {
    }

    @Override
    public IProjectConfigurator getConfig(final ProjectConfigType configType,
            final IErlProject project) {
        IProjectConfigurator result = null;
        switch (configType) {
        case INTERNAL:
            final String configName = configType.getConfigName();
            final IEclipsePreferences node = new ProjectScope(
                    project.getWorkspaceProject()).getNode(configName);
            result = new PreferencesProjectConfigurator(node);
            break;
        case REBAR:
        case EMAKE:
            result = getConfig(configType, new File(project.getWorkspaceProject()
                    .getLocation().toPortableString()));
        }
        return result;
    }

    @Override
    public IProjectConfigurator getConfig(final ProjectConfigType configType,
            final File directory) {
        IProjectConfigurator result = null;
        switch (configType) {
        case INTERNAL:
            result = new PreferencesProjectConfigurator(null);
            break;
        case REBAR:
        case EMAKE:
            final String configName = configType.getConfigName();
            final String[] resources = directory.list(new FilenameFilter() {
                @Override
                public boolean accept(final File dir, final String name) {
                    return dir.equals(directory) && name.equals(configName);
                }
            });
            if (resources.length == 0) {
                // TODO is this ok?
                return new OTPProjectConfigurator();
            }
            final String path = directory.getAbsolutePath() + "/" + resources[0];
            switch (configType) {
            case REBAR:
                result = new FileProjectConfigurator(new RebarConfigurationSerializer(),
                        path);
                break;
            case EMAKE:
                result = new FileProjectConfigurator(new EmakeConfigurationSerializer(),
                        path);
                break;
            default:
                break;
            }
        }
        return result;
    }
}
