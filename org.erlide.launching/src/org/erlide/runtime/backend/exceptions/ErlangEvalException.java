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
 * 
 * @author Vlad Dumitrescu
 */
public class ErlangEvalException extends BackendException {

	private static final long serialVersionUID = 1L;

	/**
	 * 
	 */
	public ErlangEvalException() {
		super();
	}

	/**
	 * @param message
	 */
	public ErlangEvalException(String message) {
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ErlangEvalException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param cause
	 */
	public ErlangEvalException(Throwable cause) {
		super(cause);
	}
}
