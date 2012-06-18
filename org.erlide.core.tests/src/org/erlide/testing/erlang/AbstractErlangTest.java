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

import static org.junit.Assert.*;

import org.erlide.backend.BackendCore;
import org.erlide.backend.IBackend;
import org.junit.After;
import org.junit.Before;

import com.ericsson.otp.erlang.OtpErlangObject;

public abstract class AbstractErlangTest {

    protected static final OtpErlangObject[] NO_ARGS = new OtpErlangObject[] {};

    private IBackend fBackend;

    @Before
    public void setUp() throws Exception {
        fBackend = BackendCore.getBackendManager().getIdeBackend();
    }

    @After
    public void tearDown() throws Exception {
        fBackend = null;
    }

    protected IBackend getBackend() {
        return fBackend;
    }

    public OtpErlangObject runErlangTest(final String m, final String f,
            final OtpErlangObject... args) {
        final IBackend backend = getBackend();
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
