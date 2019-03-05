package org.erlide.cover.api;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;

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
    IErlProject getProject();

    /**
     * Get all Erlang modules
     *
     * @return collection of Erlang modules
     */
    Collection<IErlModule> getModules();

    /**
     * Get module by name
     *
     * @param name
     *            module name
     * @return Erlang module
     */
    IErlModule getModule(String name);

    /**
     * Get path to project output directory (those, where beans are stored)
     *
     * @return path to output directory
     */
    IPath getOutputDir();

    /**
     * Get paths to source directories
     *
     * @return collection of paths to source directories
     */
    Collection<IPath> getSourceDirs();

    /**
     * Get paths to include directories
     *
     * @return collection of paths to include directories
     */
    Collection<IPath> getIncludeDirs();

}
