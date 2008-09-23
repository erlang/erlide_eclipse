package org.erlide.runtime.debug;


public interface IErlDebugConstants {
	final String ID_ERLANG_DEBUG_MODEL = "org.erlide.debug.model";
	final int REQUEST_REMOVE = 0;
	final int REQUEST_INSTALL = 1;

	final int DISTRIBUTED_DEBUG_FLAG = 1;
	final int ATTACH_ON_FIRST_CALL_FLAG = 2;
	final int ATTACH_ON_BREAKPOINT_FLAG = 4;
	final int ATTACH_ON_EXIT_FLAG = 8;
	final int DEFAULT_DEBUG_FLAGS = ATTACH_ON_BREAKPOINT_FLAG;
}
