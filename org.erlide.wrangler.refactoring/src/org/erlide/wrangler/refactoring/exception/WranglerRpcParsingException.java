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
 * Wrangler exception is thrown when the result of an rpc could not be parsed
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class WranglerRpcParsingException extends WranglerException {

    /**
     * Constructor
     * 
     * @param message
     *            message string
     */
    public WranglerRpcParsingException(final String message) {
        super(message);
    }

    private static final long serialVersionUID = 1L;

}
