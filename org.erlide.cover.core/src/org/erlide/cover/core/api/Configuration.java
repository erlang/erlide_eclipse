package org.erlide.cover.core.api;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.core.model.erlang.internal.OldErlangProjectProperties;
import org.erlide.cover.core.CoverException;

/**
 * Basic implementation of IConfiguration. Used to tell which modules at which
 * project should be analysed
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class Configuration implements IConfiguration {

    private IErlProject project;
    private Map<String, IErlModule> modules;

    private OldErlangProjectProperties props;
    private Logger log; // logger

    public Configuration() {
        modules = new HashMap<String, IErlModule>();
        log = Logger.getLogger(getClass());
    }

    public void setProject(String name) {
        project = ErlangCore.getModel().getErlangProject(
                ResourcesPlugin.getWorkspace().getRoot().getProject(name));
        props = new OldErlangProjectProperties(project.getProject());
    }

    public void addModule(String name) throws ErlModelException, CoverException {

        if (project == null)
            throw new CoverException("no project set");
        IErlModule module = null; // project.getModule(name);
       /* IErlModuleMap m = ErlangCore.getModuleMap();
        Set<IErlModule> mods = ErlangCore.getModuleMap().getModulesByName(name);
        module = mods.iterator().next();
        if (module != null)
            modules.put(name, module);*/
        //TODO !!!!!!!

        // else: no such module??
    }

    public void addModule(IErlModule module) {
        modules.put(module.getModuleName(), module);
    }

    public IErlProject getProject() {
        return project;
    }

    public Collection<IErlModule> getModules() {
        return modules.values();
    }

    public IPath getOutputDir() {
        return props.getOutputDir();
    }

    public Collection<IPath> getSourceDirs() {
        return props.getSourceDirs();
    }

    public Collection<IPath> getIncludeDirs() {
        return props.getIncludeDirs();
    }

    public IErlModule getModule(String name) {
        return modules.get(name);
    }

}
