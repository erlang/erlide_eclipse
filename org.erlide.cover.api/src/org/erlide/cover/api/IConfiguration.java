package org.erlide.cover.api;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlProject;

/**
 * Interface for coverage configuration
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public interface IConfiguration {

    /**
     * Get Erlang project
     * 
     * @return
     */
    public IErlProject getProject();

    /**
     * Get Erlang module
     * 
     * @return
     */
    public Collection<IErlModule> getModules();

    public IErlModule getModule(String name);

    public IPath getOutputDir();

    public Collection<IPath> getSourceDirs();

    public Collection<IPath> getIncludeDirs();

}
