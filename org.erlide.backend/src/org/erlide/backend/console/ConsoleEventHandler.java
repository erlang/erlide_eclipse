/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others. All rights reserved. This program
 * and the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.console;

import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.ErlLogger;

import com.google.common.eventbus.Subscribe;

public class ConsoleEventHandler extends ErlangEventHandler {

    private final IBackendShell shell;

    public ConsoleEventHandler(final IBackendShell backendShell) {
        super("io_server");
        shell = backendShell;
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }

        try {
            shell.add(event.getEvent());
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
    }
}
