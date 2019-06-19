package org.erlide.cover.core;

/**
 * Interface for all eunit event listeners
 *
 * @author Aleksandra Lipiec
 *
 */
public interface IEUnitObserver {

    /**
     * Reports change in the model tree when new test results arrived
     */
    void treeChanged();

    /**
     * Reports change in summary
     */
    void labelChanged();

}
