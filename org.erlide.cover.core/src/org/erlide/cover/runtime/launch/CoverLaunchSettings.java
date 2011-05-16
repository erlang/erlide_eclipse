package org.erlide.cover.runtime.launch;

import java.util.Collection;

import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.cover.api.Configuration;
import org.erlide.cover.api.CoverException;
import org.erlide.cover.api.IConfiguration;
import org.erlide.cover.core.Activator;
import org.erlide.cover.core.Logger;

/**
 * Settings for performing coverage.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverLaunchSettings {

    private final LaunchType type;
    private final FrameworkType frameworkType;
    private final Configuration config;

    private final Logger log; // logger

    /**
     * Create coverage settings, depend mainly on launch type
     * 
     * @param t
     * @param data
     */
    public CoverLaunchSettings(final LaunchType t, final CoverLaunchData data)
            throws CoverException {
        type = t;
        frameworkType = data.getFramework();
        config = new Configuration();

        log = Activator.getDefault();
        if (data.getFramework() == null) {
            throw new CoverException("No test framework set");
        }

        switch (t) {
        case MODULE:

            // new
            config.setProject(data.getProject());
            if (data.getProject() == null || data.getProject().length() == 0) {
                throw new CoverException("No project name set");
            }
            if (data.getModule() == null || data.getModule().length() == 0) {
                throw new CoverException("No module name set");
            }
            try {
                config.addModule(data.getModule().replace(".erl", ""));
            } catch (final ErlModelException e) {
                e.printStackTrace();
                throw new CoverException(e.getMessage());
            }
            break;
        case ALL:
            // new
            if (data.getProjectAll() == null
                    || data.getProjectAll().length() == 0) {
                throw new CoverException("No project name set");
            }
            config.setProject(data.getProjectAll());
            try {
                final Collection<IErlModule> allModules = config.getProject()
                        .getModules();
                for (final IErlModule m : allModules) {
                    log.info(m.getFilePath());
                    if (!m.getFilePath().contains("/test/")) { // TODO !
                        config.addModule(m);
                    }
                }
            } catch (final ErlModelException e) {
                e.printStackTrace();
                throw new CoverException(e.getMessage());
            }
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
