/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

public class RpcException extends Exception {

    private static final long serialVersionUID = -3985020542523854596L;

    public RpcException(final Exception e) {
        super(e);
    }

    public RpcException(final String string) {
        super(string);
    }

}
