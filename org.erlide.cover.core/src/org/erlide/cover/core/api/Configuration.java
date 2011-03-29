package org.erlide.cover.core.api;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;
import org.erlide.cover.core.Activator;
import org.erlide.cover.core.CoverException;
import org.erlide.cover.core.Logger;

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

    private Logger log; // logger

    public Configuration() {
        modules = new HashMap<String, IErlModule>();
        log = Activator.getDefault();
    }

    public void setProject(String name) {
        if (name == null || name.length() == 0)
            project = null;
        else
            project = ErlangCore.getModel().getErlangProject(
                    ResourcesPlugin.getWorkspace().getRoot().getProject(name));
    }

    public void addModule(String name) throws ErlModelException, CoverException {

        if (project == null)
            throw new CoverException("no project set");
        IErlModule module = null;
        module = project.getModule(name);
        modules.put(name, module);
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
        return project.getOutputLocation();
    }

    public Collection<IPath> getSourceDirs() {
        return project.getSourceDirs();
    }

    public Collection<IPath> getIncludeDirs() {
        return project.getIncludeDirs();
    }

    public IErlModule getModule(String name) {
        return modules.get(name);
    }

}
