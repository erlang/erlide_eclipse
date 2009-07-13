package org.erlide.wrangler.refactoring.core;

public abstract class SimpleWranglerRefactoring extends WranglerRefactoring {

	protected String userInput = null;

	public void setUserInput(String userInput) {
		this.userInput = userInput;
	}

}
