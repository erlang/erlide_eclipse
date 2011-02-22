package org.erlide.cover.core.api;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.preferences.OldErlangProjectProperties;
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
    private List<IErlModule> modules;

    private OldErlangProjectProperties props;
    private Logger log; // logger

    public Configuration() {
        modules = new LinkedList<IErlModule>();
        log = Logger.getLogger(getClass());
    }

    public void setProject(String name) {
        project = ErlangCore.getModel().getErlangProject(name);
        props = new OldErlangProjectProperties(project.getProject());
    }

    public void addModule(String name) throws ErlModelException, CoverException {

        if (project == null)
            throw new CoverException("no project set");
        IErlModule module = project.getModule(name);
        if (module != null)
            modules.add(module);
      
        // else: no such module??
    }

    public void addModule(IErlModule module) {
        modules.add(module);
    }

    public IErlProject getProject() {
        return project;
    }

    public List<IErlModule> getModules() {
        return modules;
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

}
