/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.jinterface.rpc;

public class BadRpcException extends RpcException {

	private static final long serialVersionUID = -3985020542523854596L;

	public BadRpcException(Exception e) {
		super(e);
	}

	public BadRpcException(String string) {
		super(string);
	}

}
