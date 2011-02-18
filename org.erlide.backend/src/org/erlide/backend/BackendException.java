/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.backend;

/**
 * Simple top level exception
 * 
 * $Revision$ $Date$
 * 
 * @author Eric Merritt
 */
public class BackendException extends Exception {

    private static final long serialVersionUID = 1L;

    /**
     * Default constructor
     */
    public BackendException() {
        super();
    }

    /**
     * Constructor with message
     * 
     * @param message
     *            The message to pass up
     */
    public BackendException(final String message) {
        super(message);
    }

    /**
     * Constructor with message and cause
     * 
     * @param message
     *            The message
     * @param cause
     *            The cause
     */
    public BackendException(final String message, final Throwable cause) {
        super(message, cause);
    }

    /**
     * Constructor with cause only
     * 
     * @param cause
     *            The cause.
     */
    public BackendException(final Throwable cause) {
        super(cause);
    }

}
