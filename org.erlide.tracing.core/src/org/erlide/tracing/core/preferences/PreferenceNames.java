package org.erlide.tracing.core.preferences;

/**
 * Class containing names of preferences used by this plugin.
 * 
 * @author Piotr Dorobisz
 * 
 */
public final class PreferenceNames {

    private PreferenceNames() {
    }

    /**
     * name of tracing node
     */
    public static String NODE_NAME = "nodeName";

    /**
     * net tick time (<a
     * href="http://www.erlang.org/doc/man/net_kernel.html#set_net_ticktime-1">
     * http://www.erlang.org/doc/man/net_kernel.html#set_net_ticktime-1</a>)
     */
    public static String TICK_TIME = "tickTime";

    /**
     * maximum number of traces that can be loaded into tree viewer
     */
    public static String TRACES_LOAD_LIMIT = "tracesLoadLimit";
}
