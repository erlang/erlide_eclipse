package org.erlide.cover.api;

/**
 * Interface for cover backend.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public interface ICoverBackend {

    /**
     * Starts cover node. Should be used before running coverage analisys.
     */
    public void startBackend();

    /**
     * Performs coverage analysis implemented by runner.
     * 
     * @param runner
     */
    public void runCoverageAnalysis(AbstractCoverRunner runner);

}
