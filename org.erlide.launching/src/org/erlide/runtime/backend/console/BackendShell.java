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
package org.erlide.runtime.backend.console;

import org.erlide.runtime.backend.IBackend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlang.ErlideReshd;

public class BackendShell {

	private final IBackend fBackend;
	private OtpErlangPid server;

	@SuppressWarnings("boxing")
	public BackendShell(IBackend backend, String id) {
		fBackend = backend;
		// fId = id;

		try {
			server = ErlideReshd.start(fBackend);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public void close() {
		if (server != null)
			fBackend.send(server, new OtpErlangAtom("stop"));
	}

	public void send(String string) {
		if (server != null)
			fBackend.send(server, new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("input"), new OtpErlangString(string) }));
	}

}
