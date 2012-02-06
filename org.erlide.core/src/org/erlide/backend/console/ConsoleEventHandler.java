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
package org.erlide.backend.console;

import org.erlide.backend.IBackend;
import org.erlide.backend.events.ErlangEventHandler;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangObject;

public class ConsoleEventHandler extends ErlangEventHandler {

    private final IBackendShell shell;

    public ConsoleEventHandler(final IBackend backend,
            final IBackendShell backendShell) {
        super("io_server", backend);
        shell = backendShell;
    }

    @Override
    public void handleEvent(final Event event) {
        shell.add((OtpErlangObject) event.getProperty("DATA"));
    }
}
