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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;

import org.eclipse.debug.core.IStreamListener;
import org.erlide.runtime.backend.IBackend;

import erlang.ErlideReshd;

public class BackendShell {

	private final IBackend fBackend;

	/* Not used */
	// private String fId;
	Socket fSocket = null;

	@SuppressWarnings("boxing")
	public BackendShell(IBackend backend, String id) {
		fBackend = backend;
		// fId = id;

		try {
			final int port = ErlideReshd.start(fBackend);
			// TODO use backend.getHost()
			fSocket = new Socket("localhost", port);
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	public void close() {
		if (fSocket == null) {
			return;
		}
		try {
			fSocket.close();
		} catch (final IOException e) {
		}
	}

	public OutputStream getOutputStream() {
		if (fSocket == null) {
			return null;
		}
		try {
			return fSocket.getOutputStream();
		} catch (final IOException e) {
			return null;
		}
	}

	public InputStream getInputStream() {
		if (fSocket == null) {
			return null;
		}
		try {
			return fSocket.getInputStream();
		} catch (final IOException e) {
			return null;
		}
	}

	public void setHandler(IStreamListener stdHandler) {
		final OutputStreamMonitor m = new OutputStreamMonitor(getInputStream());
		m.addListener(stdHandler);
		m.startMonitoring();
	}

}
