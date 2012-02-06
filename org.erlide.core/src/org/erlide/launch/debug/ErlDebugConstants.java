package org.erlide.launch.debug;

/**
 * @noextend This interface is not intended to be extended by clients.
 * @noimplement This interface is not intended to be implemented by clients.
 */
public final class ErlDebugConstants {
    public static final String ID_ERLANG_DEBUG_MODEL = "org.erlide.debug.model";
    public static final int REQUEST_REMOVE = 0;
    public static final int REQUEST_INSTALL = 1;

    public static final int DISTRIBUTED_DEBUG = 1;
    public static final int ATTACH_ON_FIRST_CALL = 2;
    public static final int ATTACH_ON_BREAKPOINT = 4;
    public static final int ATTACH_ON_EXIT = 8;
    public static final int DEFAULT_DEBUG_FLAGS = ATTACH_ON_BREAKPOINT;

    private ErlDebugConstants() {
    }
}
