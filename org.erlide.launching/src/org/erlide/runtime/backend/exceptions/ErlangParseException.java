/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend.exceptions;

/**
 * 
 * @author Vlad Dumitrescu
 */
public class ErlangParseException extends BackendException {

	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public ErlangParseException() {
		super();
	}

	/**
	 * @param message
	 */
	public ErlangParseException(String message) {
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ErlangParseException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param cause
	 */
	public ErlangParseException(Throwable cause) {
		super(cause);
	}
}
