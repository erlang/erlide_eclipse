package org.erlide.cover.core;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.FrameworkType;
import org.erlide.cover.runtime.launch.LaunchType;

/**
 * Settings for performing coverage.
 * 
 * @author Aleksandra Lipiec
 *
 */
public class CoverSettings {
    
    private LaunchType type;
    private Map<String, String> paths;
    private FrameworkType frameworkType;

    /**
     * Create coverage settings, depend mainly on launch type
     * @param t
     * @param data
     */
    public CoverSettings(LaunchType t, CoverLaunchData data) {
        paths = new HashMap<String, String>();
        type = t;
        frameworkType = data.getFramework();
        
        if(data == null)
            return;
        
        switch(t) {
        case MODULE: 
            IProject p = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(data.getProject());
            String path = p.getLocation().toString() + "/src/" + data.getModule();
            //TODO: better mechanizm for finding file paths within project
            paths.put(data.getModule().replace(".erl", ""), path);
            break;
        case ALL:
            path = ResourcesPlugin.getWorkspace().
                    getRoot().getRawLocation().toString();
            path += "/" + data.getFile();
            System.out.println(path);
            paths.put( "", path);
            break;
        case APPLICATION:
            //TODO: finding application - should be simmilar to finding module
            break;
        case CUSTOM:
            //TODO: custom settings - list of files(?), tests(?)
            break;            
        }
    }
    
    public LaunchType getType() {
        return type;
    }
    
    public String getTypeAsString() {
        return type.name().toLowerCase();
    }

    public Set<String> modules() {
        return paths.keySet();
    }
    
    public String getPath(String module) {
        return paths.get(module);
    }
    
    public void addPath(String module, String path) {
        paths.put(module, path);
    }
    
    public void addPaths(List<String> files) {
        //TODO:prepare data
        //paths.addAll(files);
    }
    
    public String getFramework() {
        return frameworkType.name().toLowerCase();
    }
    

}
