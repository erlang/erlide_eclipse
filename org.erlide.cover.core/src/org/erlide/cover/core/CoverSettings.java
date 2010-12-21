package org.erlide.cover.core;

import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.LaunchType;

/**
 * Settings for performing coverage.
 * 
 * @author Aleksandra Lipiec
 *
 */
public class CoverSettings {
    
    private LaunchType type;
    private List<String> paths; 
    
    /**
     * Create coverage settings, depend mainly on launch type
     * @param t
     * @param data
     */
    public CoverSettings(LaunchType t, CoverLaunchData data) {
        paths = new LinkedList<String>();
        type = t;
        
        if(data == null)
            return;
        
        switch(t) {
        case MODULE: 
            IProject p = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(data.getProject());
            String path = p.getLocation().toString() + "/src";
            //TODO: better mechanizm for finding file paths within project
            paths.add(path);
            break;
        case ALL:
            //TODO: getting path for directories / files
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

    public List<String> getPaths() {
        return paths;
    }
    
    public void addPath(String path) {
        paths.add(path);
    }
    
    public void addPaths(List<String> files) {
        paths.addAll(files);
    }
    

}
