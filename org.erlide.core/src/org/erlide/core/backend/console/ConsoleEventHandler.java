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
package org.erlide.core.backend.console;

import org.erlide.core.backend.events.ErlangEvent;
import org.erlide.core.backend.events.EventHandler;

public class ConsoleEventHandler extends EventHandler {

    private final BackendShell shell;
    private final String nodeName;

    public ConsoleEventHandler(final BackendShell backendShell,
            final String nodeName) {
        shell = backendShell;
        this.nodeName = nodeName;
    }

    @Override
    protected void doHandleEvent(final ErlangEvent event) throws Exception {
        if (!event.matchTopicAndNode("io_server", nodeName)) {
            return;
        }
        shell.add(event.data);
    }
}
