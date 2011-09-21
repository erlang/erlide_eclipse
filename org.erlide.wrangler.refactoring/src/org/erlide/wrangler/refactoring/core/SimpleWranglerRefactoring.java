/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
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
     *            user input data
     */
    public void setUserInput(final String userInput) {
        this.userInput = userInput;
    }

}
