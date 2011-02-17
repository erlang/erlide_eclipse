package org.erlide.cover.core.api;

import java.util.Collection;
import java.util.List;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;

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
    public List<IErlModule> getModules();
    
    public IPath getOutputDir();
    
    public Collection<IPath> getSourceDirs();
    
    public Collection<IPath> getIncludeDirs();

}
