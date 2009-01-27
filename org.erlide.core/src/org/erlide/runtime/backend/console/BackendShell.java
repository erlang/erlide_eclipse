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

import org.erlide.jinterface.JInterfaceFactory;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.Backend;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;

import erlang.ErlideReshd;

public class BackendShell {

	private final Backend fBackend;
	private OtpErlangPid server;

	public BackendShell(Backend backend, String id) {
		fBackend = backend;
		// fId = id;

		try {
			server = ErlideReshd.start(fBackend);
		} catch (final Exception e) {
			ErlLogger.warn(e);
		}
	}

	public void close() {
		if (server != null) {
			fBackend.send(server, new OtpErlangAtom("stop"));
		}
	}

	public void send(String string) {
		if (server != null) {
			fBackend.send(server, JInterfaceFactory.mkTuple(new OtpErlangAtom(
					"input"), new OtpErlangString(string)));
		}
	}

}
