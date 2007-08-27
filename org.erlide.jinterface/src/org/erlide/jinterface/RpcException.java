/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface;

public class RpcException extends Exception {

	private static final long serialVersionUID = -3985020542523854596L;

	public RpcException(Exception e) {
		super(e);
	}

	public RpcException(String string) {
		super(string);
	}

}
