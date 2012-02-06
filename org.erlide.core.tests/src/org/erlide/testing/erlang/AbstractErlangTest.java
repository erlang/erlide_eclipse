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

import org.erlide.backend.BackendCore;
import org.erlide.jinterface.rpc.IRpcCallSite;
import org.junit.After;
import org.junit.Before;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class AbstractErlangTest {

    protected static final OtpErlangObject[] NO_ARGS = new OtpErlangObject[] {};

    private IRpcCallSite fBackend;

    @Before
    public void setUp() throws Exception {
        fBackend = BackendCore.getBackendManager().getIdeBackend();
    }

    @After
    public void tearDown() throws Exception {
        fBackend = null;
    }

    protected IRpcCallSite getBackend() {
        return fBackend;
    }

    public OtpErlangObject runErlangTest(final String m, final String f,
            final OtpErlangObject... args) {
        final IRpcCallSite backend = getBackend();
        if (backend == null) {
            fail("Backend is null");
            return null;
        }
        try {
            return backend.call(m, f, null, (Object[]) args);
        } catch (final Exception e) {
            e.printStackTrace();
            fail("RPC failed: " + e.getMessage());
        }
        return null;
    }

}
