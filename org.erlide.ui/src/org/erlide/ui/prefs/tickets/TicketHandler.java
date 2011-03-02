/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.tickets;

import java.net.MalformedURLException;
import java.net.URL;

import org.erlide.ui.prefs.ProblemData;

public interface TicketHandler {

    TicketStatus send(ProblemData info);

    String infoToMessage(ProblemData info);

    TicketStatus parseMessage(String message);

    URL getLoginURL(String user, String pass) throws MalformedURLException;
}
