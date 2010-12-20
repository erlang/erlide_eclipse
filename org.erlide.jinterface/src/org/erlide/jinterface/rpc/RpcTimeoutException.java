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

public class RpcTimeoutException extends RpcException {
    private static final long serialVersionUID = 1229604283654671393L;

    public RpcTimeoutException() {
        super("timeout");
    }

    public RpcTimeoutException(final String msg) {
        super("timeout in " + msg);
    }

}
