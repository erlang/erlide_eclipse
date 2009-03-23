/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;


/**
 * This is a tread driven by a mailbox, that waits for rpc results and
 * dispatches them to registered callback.
 * 
 * Protocol:
 * <ul>
 * <li>caller starts receiver and registers callback</li>
 * <li>caller sends request via rpc:cast()</li>
 * <li>receiver waits for "start" message; calls callback</li>
 * <li>receiver waits for "progress" messages; calls callback for each one</li>
 * <li>receiver waits for "stop" message, calls callback and quits</li>
 * </ul>
 */
public class RpcResultReceiver implements Runnable {

	private RpcResultCallback callback;

	public RpcResultReceiver(RpcResultCallback callback) {
		this.callback = callback;
		new Thread(this, "rpc");
	}

	public void run() {
		// OtpMbox box
	}

}
