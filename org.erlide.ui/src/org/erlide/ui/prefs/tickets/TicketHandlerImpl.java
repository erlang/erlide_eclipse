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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;

public abstract class TicketHandlerImpl implements TicketHandler {

	public TicketHandlerImpl() {
		super();
	}

	public TicketStatus send(TicketInfo info) {
		String msg = infoToMessage(info);
		URL url;
		String resp;
		try {
			url = getLoginURL("", "");
			login(url);
			url = getURL();
			resp = post(url, msg);
			TicketStatus result = parseMessage(resp);
			return result;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}

	private URL getURL() throws MalformedURLException {
		return new URL("http://www.assembla.com/spaces/erlide/tickets/");
	}

	private String post(final URL url, final String message) throws IOException {
		Proxy proxy = Proxy.NO_PROXY;
		if ("true".equals(System.getProperty("proxySet"))) {
			final String host = System.getProperty("proxyHost");
			final int port = Integer.parseInt(System.getProperty("proxyPort"));
			final InetSocketAddress addr = new InetSocketAddress(host, port);
			proxy = new Proxy(Proxy.Type.HTTP, addr);
		}
		URLConnection conn;

		conn = url.openConnection();
		conn.setDoOutput(true);
		OutputStreamWriter wr = new OutputStreamWriter(conn.getOutputStream());
		wr.write(message);
		wr.flush();

		// Get the response
		BufferedReader rd = new BufferedReader(new InputStreamReader(conn
				.getInputStream()));
		String line;
		while ((line = rd.readLine()) != null) {
			// Process line...
			System.out.println(line);
		}
		wr.close();
		rd.close();

		return "??";
	}

	private void login(final URL url) throws IOException {
		Proxy proxy = Proxy.NO_PROXY;
		if ("true".equals(System.getProperty("proxySet"))) {
			final String host = System.getProperty("proxyHost");
			final int port = Integer.parseInt(System.getProperty("proxyPort"));
			final InetSocketAddress addr = new InetSocketAddress(host, port);
			proxy = new Proxy(Proxy.Type.HTTP, addr);
		}
		URLConnection conn;

		conn = url.openConnection();

		// Get the response
		BufferedReader rd = new BufferedReader(new InputStreamReader(conn
				.getInputStream()));
		String line;
		while ((line = rd.readLine()) != null) {
			// Process line...
			System.out.println(line);
		}
		rd.close();
	}

}
