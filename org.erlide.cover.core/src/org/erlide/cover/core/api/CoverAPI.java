package org.erlide.cover.core.api;

import java.util.Collection;

import org.erlide.cover.core.CoverBackend;

/**
 * API for using cover plugin
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public interface CoverAPI {
    
    /**
     * Start cover on the nodes given
     * 
     * if cover is already started on the given nodes nothing happen
     * 
     * if cover is already started and the node list changed,
     * it is started once again
     * 
     * @param nodes
     */
    public void startCover(Collection<String> nodes);
    
    /**
     * Sets coverage configuration, and prepares all selected files
     * 
     * @param conf
     */
    public void setCoverageConfiguration(IConfiguration conf);
    
    /**
     * Perform cover analises based on present configuration
     */
    public void analyse();

}
