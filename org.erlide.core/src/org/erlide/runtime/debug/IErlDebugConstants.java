package org.erlide.runtime.debug;


public interface IErlDebugConstants {
	final String ID_ERLANG_DEBUG_MODEL = "org.erlide.debug.model";
	final int REQUEST_REMOVE = 0;
	final int REQUEST_INSTALL = 1;

	final int DISTRIBUTED_DEBUG = 1;
	final int ATTACH_ON_FIRST_CALL = 2;
	final int ATTACH_ON_BREAKPOINT = 4;
	final int ATTACH_ON_EXIT = 8;
	final int DEFAULT_DEBUG_FLAGS = ATTACH_ON_BREAKPOINT;
}
