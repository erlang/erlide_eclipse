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

import static org.junit.Assert.fail;

import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IBackend;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.junit.After;
import org.junit.Before;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class AbstractErlangTest {

	protected static final OtpErlangObject[] NO_ARGS = new OtpErlangObject[] {};

	private IBackend fBackend;

	@Before
	public void setUp() throws Exception {
		fBackend = BackendManager.getDefault().getIdeBackend();
	}

	@After
	public void tearDown() throws Exception {
		fBackend = null;
	}

	protected IBackend getBackend() {
		return fBackend;
	}

	public OtpErlangObject runErlangTest(String m, String f,
			OtpErlangObject... args) {
		try {
			return getBackend().rpcx(m, f, (Object[]) args);
		} catch (ErlangRpcException e) {
			fail("RPC failed: " + e.getMessage());
		} catch (BackendException e) {
			fail("RPC failed: " + e.getMessage());
		}
		return null;
	}

}
