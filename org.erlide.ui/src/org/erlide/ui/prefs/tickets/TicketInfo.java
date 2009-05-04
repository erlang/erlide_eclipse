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

public class TicketInfo {
	public String summary;
	public String reporter;
	public String description;
	public String platformLog;
	public String erlideLog;

	public TicketInfo(String title, String body, String contact, String plog,
			String elog) {
		this.summary = title;
		this.reporter = contact;
		this.description = body;
		this.platformLog = plog;
		this.erlideLog = elog;
	}

}
