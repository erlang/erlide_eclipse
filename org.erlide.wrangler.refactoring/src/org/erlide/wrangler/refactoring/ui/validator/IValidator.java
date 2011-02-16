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
package org.erlide.wrangler.refactoring.ui.validator;

/**
 * Interface for validator classes. Implementors should implemet a class which
 * is for validatin g a string.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public interface IValidator {
    /**
     * Validate a string
     * 
     * @param text
     *            input string
     * @return true if the string is valid, else false
     */
    boolean isValid(String text);
}
