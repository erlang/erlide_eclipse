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

	public void setUserInput(String userInput) {
		this.userInput = userInput;
	}

}
