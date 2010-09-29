package org.erlide.tracing.core;

/**
 * Abstract class containing constants.
 * 
 * @author Piotr Dorobisz
 * 
 */
public abstract class Constants {

    /**
     * logs file name
     */
    public static final String OUTPUT_FILE = "erlide_tracing";

    /**
     * function that converts strings into match specifications
     */
    public static final String FUN_STR2MS = "str2ms";

    /**
     * module containing helper functions
     */
    public static final String ERLANG_HELPER_MODULE = "ttb_integration";

    /**
     * tracing tool module
     */
    public static final String TTB_MODULE = "ttbe";
}
