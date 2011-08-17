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
package org.erlide.wrangler.refactoring.exception;

/**
 * Absrtact class for exceptions used by the refactor plug-in.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerException extends Exception {

    /**
     * ID
     */
    private static final long serialVersionUID = 6955527507414603986L;

    /**
     * Constructor
     * 
     * @param message
     *            message string
     */
    public WranglerException(final String message) {
        super(message);
    }

}
