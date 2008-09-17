package org.erlide.wrangler.refactoring.core.exception;


/**
 * This exception indicates that some error occured during the communication
 * with the wrangler. Usually there is a problem with the Erlang node.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerRPCException extends WranglerException {

	/**
	 * Sole constructor with built-in message
	 */
	public WranglerRPCException() {
		super("Could not reach the Erlang node!");
	}

}
