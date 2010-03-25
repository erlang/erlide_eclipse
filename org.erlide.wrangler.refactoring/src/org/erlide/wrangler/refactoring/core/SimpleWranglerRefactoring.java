package org.erlide.wrangler.refactoring.core;

/**
 * Abstract class for implementing wrangler refactorings which has a simple
 * workflow.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class SimpleWranglerRefactoring extends WranglerRefactoring {

	protected String userInput = null;

	/**
	 * Most of the refactorings needs an input parameter (e.g. new name). This
	 * function is for setting this input.
	 * 
	 * @param userInput
	 */
	public void setUserInput(String userInput) {
		this.userInput = userInput;
	}

}
