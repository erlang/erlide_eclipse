package org.erlide.tracing.core;

/**
 * Elements of this enum describe possible results of starting/stopping tracing
 * and loading traces from disk.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum TracingStatus {

    /**
     * action finished successfully but no data was received
     */
    EMPTY,
    /**
     * action finished with error on Erlang side
     */
    ERROR,

    /**
     * exception was thrown
     */
    EXCEPTION_THROWN,

    /**
     * no nodes were activated for tracing
     */
    NO_ACTIVATED_NODES,

    /**
     * some nodes were not activated for tracing
     */
    NOT_ALL_NODES_ACTIVATED,

    /**
     * action finished successfully
     */
    OK;
}
