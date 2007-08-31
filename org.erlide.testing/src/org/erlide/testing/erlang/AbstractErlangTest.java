/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.testing.erlang;

import junit.framework.TestCase;

import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.RpcResult;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class AbstractErlangTest extends TestCase {

	protected static final OtpErlangObject[] NO_ARGS = new OtpErlangObject[] {};

	private IBackend fBackend;

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		fBackend = BackendManager.getDefault().getIdeBackend();
	}

	@Override
	protected void tearDown() throws Exception {
		fBackend = null;
		super.tearDown();
	}

	protected IBackend getBackend() {
		return fBackend;
	}

	public OtpErlangObject runErlangTest(String m, String f,
			OtpErlangObject... args) {
		try {
			return getBackend().rpc(m, f, (Object) args).getValue();
		} catch (ErlangRpcException e) {
			fail("RPC failed: " + e.getMessage());
		}
		return null;
	}

	public void erlangTest(String m, String f, OtpErlangObject... args) {
		try {
			RpcResult r = getBackend().rpc(m, f, (Object[]) args);
			System.out.println("RRR=" + r.getValue());
			assertNotNull(r);
			// assertTrue(r.isOk());
		} catch (ErlangRpcException e) {
			fail("RPC failed: " + e.getMessage());
		}
	}

}
