package org.erlide.wrangler.refactoring.core.exception;

/**
 * Absrtact class for exceptions used by the refactor plug-in.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected WranglerException(String message) {
		super(message);
	}

}
