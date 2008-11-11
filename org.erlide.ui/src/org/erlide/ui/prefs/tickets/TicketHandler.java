/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs.tickets;

import java.net.MalformedURLException;
import java.net.URL;

public interface TicketHandler {

	TicketStatus send(TicketInfo info);

	String infoToMessage(TicketInfo info);

	TicketStatus parseMessage(String message);

	URL getLoginURL(String user, String pass) throws MalformedURLException;
}
