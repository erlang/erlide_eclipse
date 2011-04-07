package org.erlide.cover.api;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.CoreScope;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;

/**
 * Basic implementation of IConfiguration. Used to tell which modules at which
 * project should be analysed
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class Configuration implements IConfiguration {

    private IErlProject project;
    private final Map<String, IErlModule> modules;

    public Configuration() {
        modules = new HashMap<String, IErlModule>();
    }

    public void setProject(final String name) {
        if (name == null || name.length() == 0) {
            project = null;
        } else {
            project = CoreScope.getModel().getErlangProject(
                    ResourcesPlugin.getWorkspace().getRoot().getProject(name));
        }
    }

    public void addModule(final String name) throws ErlModelException,
            CoverException {

        if (project == null) {
            throw new CoverException("no project set");
        }
        IErlModule module = null;
        module = project.getModule(name);
        modules.put(name, module);
    }

    public void addModule(final IErlModule module) {
        modules.put(module.getModuleName(), module);
    }

    @Override
    public IErlProject getProject() {
        return project;
    }

    @Override
    public Collection<IErlModule> getModules() {
        return modules.values();
    }

    @Override
    public IPath getOutputDir() {
        return project.getOutputLocation();
    }

    @Override
    public Collection<IPath> getSourceDirs() {
        return project.getSourceDirs();
    }

    @Override
    public Collection<IPath> getIncludeDirs() {
        return project.getIncludeDirs();
    }

    @Override
    public IErlModule getModule(final String name) {
        return modules.get(name);
    }

}
