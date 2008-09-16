package org.erlide.wrangler.refactoring.core.exception;

/**
 * This exception indicates some error during the refactoring process. Usually:
 * wrong variable name, clashing functions, etc.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerRefactoringException extends WranglerException {

	/**
	 * Sole constructor
	 * 
	 * @param message
	 *            sent error message
	 */
	public WranglerRefactoringException(String message) {
		super(message);
	}

}
