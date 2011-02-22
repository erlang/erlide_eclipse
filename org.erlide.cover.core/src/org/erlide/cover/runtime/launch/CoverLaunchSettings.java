package org.erlide.cover.runtime.launch;

import java.util.Collection;

import org.apache.log4j.Logger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlModule;
import org.erlide.cover.core.CoverException;
import org.erlide.cover.core.api.Configuration;
import org.erlide.cover.core.api.IConfiguration;

/**
 * Settings for performing coverage.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchSettings {

    private final LaunchType type;
    private final FrameworkType frameworkType;
    private Configuration config;

    private Logger log; // logger

    /**
     * Create coverage settings, depend mainly on launch type
     * 
     * @param t
     * @param data
     */
    public CoverLaunchSettings(LaunchType t, CoverLaunchData data) {
        type = t;
        frameworkType = data.getFramework();
        config = new Configuration();

        log = Logger.getLogger(getClass());

        switch (t) {
        case MODULE:

            // new
            config.setProject(data.getProject());
            try {
                config.addModule(data.getModule().replace(".erl", ""));
            } catch (ErlModelException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            } catch (CoverException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            break;
        case ALL:
            // new
            config.setProject(data.getFile());
            try {
                Collection<IErlModule> allModules = config.getProject()
                        .getModules();
                for (IErlModule m : allModules) {
                    log.debug(m.getFilePath());
                    if (!m.getFilePath().contains("/test/")) { // TODO !
                        config.addModule(m);
                    }
                }
            } catch (ErlModelException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            break;
        case APPLICATION:
            break;
        case CUSTOM:
            break;
        }
        
    }

    public LaunchType getType() {
        return type;
    }

    public String getTypeAsString() {
        return type.name().toLowerCase();
    }

    public String getFramework() {
        return frameworkType.name().toLowerCase();
    }

    public IConfiguration getConfig() {
        return config;
    }

}
