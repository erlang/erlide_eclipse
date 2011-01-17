package org.erlide.cover.core;

import java.util.LinkedList;
import java.util.List;
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
    private List<CoverObject> objs;
    private FrameworkType frameworkType;

    /**
     * Create coverage settings, depend mainly on launch type
     * @param t
     * @param data
     */
    public CoverSettings(LaunchType t, CoverLaunchData data) {
        objs = new LinkedList<CoverObject>();
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
            objs.add(new CoverObject(CoverObject.MODULE, 
                    data.getModule().replace(".erl", ""), path));
            break;
        case ALL:
            path = ResourcesPlugin.getWorkspace().
                    getRoot().getRawLocation().toString();
            path += "/" + data.getFile();
            System.out.println(path);
            objs.add(new CoverObject(CoverObject.DIR, path));
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

    public List<CoverObject> objects() {
        return objs;
    }
        
    public String getFramework() {
        return frameworkType.name().toLowerCase();
    }
    

}
