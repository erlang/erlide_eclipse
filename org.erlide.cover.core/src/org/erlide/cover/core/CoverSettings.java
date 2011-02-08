package org.erlide.cover.core;

import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.erlide.cover.runtime.launch.CoverLaunchData;
import org.erlide.cover.runtime.launch.FrameworkType;
import org.erlide.cover.runtime.launch.LaunchType;

/**
 * Settings for performing coverage.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverSettings {

    private final LaunchType type;
    private final List<CoverObject> objs;
    private final FrameworkType frameworkType;
    
    private Logger log;     //logger

    /**
     * Create coverage settings, depend mainly on launch type
     * 
     * @param t
     * @param data
     */
    public CoverSettings(final LaunchType t, final CoverLaunchData data) {
        objs = new LinkedList<CoverObject>();
        type = t;
        frameworkType = data.getFramework();
        
        log = Logger.getLogger(getClass());

        switch (t) {
        case MODULE:
            final IProject p = ResourcesPlugin.getWorkspace().getRoot()
                    .getProject(data.getProject());
            final String path = p.getLocation().toString() + "/src/"
                    + data.getModule();
            // TODO: better mechanizm for finding file paths within project
            String pathEbin = p.getLocation().toString() + "/ebin";
            objs.add(new CoverObject(CoverObject.MODULE, data.getModule()
                    .replace(".erl", ""), path, path, pathEbin));
            break;
        case ALL:
            log.debug(data);
            log.debug("all: " + data.getFile());
            final StringBuffer bf = new StringBuffer(ResourcesPlugin
                    .getWorkspace().getRoot().getRawLocation().toString());
            bf.append("/").append(data.getFile()).append("/");

            log.debug(bf);
            final String pathSrc = bf.toString() + "src";
            final String pathTst = bf.toString() + "test";
            pathEbin = bf.toString() + "ebin";
            objs.add(new CoverObject(CoverObject.PROJ, pathSrc, pathTst,
                    pathEbin));
            break;
        case APPLICATION:
            // TODO: finding application - should be simmilar to finding module
            break;
        case CUSTOM:
            // TODO: custom settings - list of files(?), tests(?)
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
