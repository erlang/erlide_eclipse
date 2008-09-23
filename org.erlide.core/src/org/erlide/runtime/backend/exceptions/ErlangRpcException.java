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
public class ErlangRpcException extends BackendException {

	private static final long serialVersionUID = 1L;

	/**
	 * 
	 * 
	 */
	public ErlangRpcException() {
		super();
	}

	/**
	 * 
	 * @param message
	 */
	public ErlangRpcException(String message) {
		super(message);
	}

	/**
	 * 
	 * @param message
	 * @param cause
	 */
	public ErlangRpcException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * 
	 * @param cause
	 */
	public ErlangRpcException(Throwable cause) {
		super(cause);
	}
}
