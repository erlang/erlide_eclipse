package org.erlide.cover.runtime.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;

/**
 * Stores launch configuration data
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchData {

    private final LaunchType type;
    private final String project;
    private final String module;
    private final String file;
    private final String appProject;
    private String application;
    private final FrameworkType framework;

    public CoverLaunchData(final ILaunchConfiguration config)
            throws CoreException {

        type = LaunchType.valueOf(config.getAttribute(ICoverAttributes.TYPE,
                LaunchType.MODULE.toString()));

        project = config.getAttribute(ICoverAttributes.PROJECT, "");
        module = config.getAttribute(ICoverAttributes.MODULE, "");
        file = config.getAttribute(ICoverAttributes.FILE, "");
        appProject = config.getAttribute(ICoverAttributes.APP_PROJECT, "");

        framework = FrameworkType.find(config.getAttribute(
                ICoverAttributes.COMBO, FrameworkType.EUNIT.getRepr()));

    }

    public LaunchType getType() {
        return type;
    }

    public String getProject() {
        return project;
    }

    public String getModule() {
        return module;
    }

    public String getProjectAll() {
        return file;
    }

    public String getAppProject() {
        return appProject;
    }

    public String getApp() {
        return application;
    }

    public FrameworkType getFramework() {
        return framework;
    }

}
