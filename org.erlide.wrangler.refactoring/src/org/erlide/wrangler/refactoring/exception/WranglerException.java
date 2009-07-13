package org.erlide.wrangler.refactoring.exception;

/**
 * Absrtact class for exceptions used by the refactor plug-in.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6955527507414603986L;

	public WranglerException(String message) {
		super(message);
	}

}
