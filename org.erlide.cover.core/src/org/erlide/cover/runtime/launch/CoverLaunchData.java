package org.erlide.cover.runtime.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;

public class CoverLaunchData {

    private LaunchType type;
    private String project;
    private String module;
    private String file;
    private String appProject;
    private String application;
    private FrameworkType framework;
    
    public CoverLaunchData(ILaunchConfiguration config)
            throws CoreException {
        
        System.out.println("Cover launch data!");
        
        type = LaunchType.valueOf(config.getAttribute(ICoverAttributes.TYPE,
                LaunchType.MODULE.toString()));
        
        project = config.getAttribute(ICoverAttributes.PROJECT, "");
        module = config.getAttribute(ICoverAttributes.MODULE, "");
        file = config.getAttribute(ICoverAttributes.FILE, "");
        appProject = config.getAttribute(ICoverAttributes.APP_PROJECT, "");
        System.out.println("cover launch data " + config.getAttribute(ICoverAttributes.COMBO, 
                FrameworkType.EUNIT.getRepr()));
        framework = FrameworkType.find(
                config.getAttribute(ICoverAttributes.COMBO, 
                        FrameworkType.EUNIT.getRepr()));
        
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
    
    public String getFile() {
        return file;
    }
    
    public String getAppProject() {
        return appProject;
    }
    
    public String getApp () {
        return application;
    }
    
    public FrameworkType getFramework() {
        return framework;
    }

}
