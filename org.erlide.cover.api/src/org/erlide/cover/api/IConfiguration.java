package org.erlide.cover.api;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.IErlProject;

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
     * @return Erlang project
     */
    public IErlProject getProject();

    /**
     * Get all Erlang modules
     * 
     * @return collection of Erlang modules
     */
    public Collection<IErlModule> getModules();

    /**
     * Get module by name
     * 
     * @param name
     *            module name
     * @return Erlang module
     */
    public IErlModule getModule(String name);

    /**
     * Get path to project output directory (those, where beans are stored)
     * 
     * @return path to output directory
     */
    public IPath getOutputDir();

    /**
     * Get paths to source directories
     * 
     * @return collection of paths to source directories
     */
    public Collection<IPath> getSourceDirs();

    /**
     * Get paths to include directories
     * 
     * @return collection of paths to include directories
     */
    public Collection<IPath> getIncludeDirs();

}
