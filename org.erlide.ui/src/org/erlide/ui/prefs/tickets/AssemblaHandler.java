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

public class AssemblaHandler extends TicketHandlerImpl {

	static final String MYURL = "http://www.assembla.com/spaces/erlide/tickets/";
	static final String LOGINURL = "https://www.assembla.com/users/login";

	public String infoToMessage(TicketInfo info) {
		return "<ticket><summary>" + info.summary + "</summary></ticket>";
	}

	public TicketStatus parseMessage(String message) {
		boolean ok = false;
		int id = 0;

		TicketStatus result = new TicketStatus(ok, id);
		return result;
	}

	public URL getLoginURL(String user, String pass)
			throws MalformedURLException {
		String str = String.format(MYURL, user, pass);
		return new URL(str);
	}

}
