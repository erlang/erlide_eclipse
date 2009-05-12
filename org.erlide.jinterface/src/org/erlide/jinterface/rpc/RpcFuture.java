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


import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpMbox;

public class RpcFuture {

	private final OtpMbox mbox;
	private OtpErlangObject result = null;

	public RpcFuture(final OtpMbox mbox) {
		this.mbox = mbox;
	}

	public OtpErlangObject get() throws RpcException {
		if (isDone()) {
			return result;
		}
		return get(RpcUtil.INFINITY);
	}

	public OtpErlangObject get(final long timeout) throws RpcException {
		if (isDone()) {
			return result;
		}
		result = RpcUtil.getRpcResult(mbox, timeout);
		return result;
	}

	public boolean isDone() {
		return result != null;
	}

}
