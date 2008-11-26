package org.erlide.runtime.debug;

public class ErlDebugConstants {
	static final public String ID_ERLANG_DEBUG_MODEL = "org.erlide.debug.model";
	static final public int REQUEST_REMOVE = 0;
	static final public int REQUEST_INSTALL = 1;

	static final public int DISTRIBUTED_DEBUG = 1;
	static final public int ATTACH_ON_FIRST_CALL = 2;
	static final public int ATTACH_ON_BREAKPOINT = 4;
	static final public int ATTACH_ON_EXIT = 8;
	static final public int DEFAULT_DEBUG_FLAGS = ATTACH_ON_BREAKPOINT;

	private ErlDebugConstants() {
	}
}
