package org.erlide.tracing.core;

/**
 * Enum representing trace types (e.g. <i>send</i>, <i>receive</i>).
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum TraceType {
    //@formatter:off
    CALL,
    EXCEPTION_FROM,
    EXIT,
    GC_END,
    GC_START,
    GETTING_LINKED,
    GETTING_UNLINKED,
    IN,
    LINK,
    OUT,
    RECEIVE,
    REGISTER,
    RETURN_FROM,
    RETURN_TO,
    SEND,
    SEND_TO_NON_EXISTING_PROCESS,
    SPAWN,
    UNLINK,
    UNREGISTER;
    //@formatter:on
}
