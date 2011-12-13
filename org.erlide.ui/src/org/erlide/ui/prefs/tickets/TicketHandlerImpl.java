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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;

import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.prefs.ProblemData;

public abstract class TicketHandlerImpl implements TicketHandler {

    public TicketHandlerImpl() {
        super();
    }

    @Override
    public TicketStatus send(final ProblemData info) {
        final String msg = infoToMessage(info);
        try {
            URL url = getLoginURL(
                    System.getProperty("erlide.assembla.user", ""),
                    System.getProperty("erlide.assembla.password", ""));
            if (url == null) {
                return new TicketStatus(false, 0);
            }
            login(url);
            url = getURL();
            final String resp = post(url, msg);
            final TicketStatus result = parseMessage(resp);
            return result;
        } catch (final IOException e) {
            ErlLogger.warn(e);
            return new TicketStatus(false, 0);
        }
    }

    private URL getURL() throws MalformedURLException {
        return new URL("http://www.assembla.com/spaces/erlide/tickets/");
    }

    private String post(final URL url, final String message) throws IOException {
        Proxy proxy = Proxy.NO_PROXY;
        if (Boolean.parseBoolean(System.getProperty("proxySet"))) {
            final String host = System.getProperty("proxyHost");
            final int port = Integer.parseInt(System.getProperty("proxyPort"));
            final InetSocketAddress addr = new InetSocketAddress(host, port);
            proxy = new Proxy(Proxy.Type.HTTP, addr);
        }
        URLConnection conn;

        conn = url.openConnection(proxy);
        conn.setDoOutput(true);
        final OutputStreamWriter wr = new OutputStreamWriter(
                conn.getOutputStream());
        wr.write(message);
        wr.flush();

        // Get the response
        final BufferedReader rd = new BufferedReader(new InputStreamReader(
                conn.getInputStream()));
        String line;
        while ((line = rd.readLine()) != null) {
            // Process line...
            ErlLogger.debug(line);
        }
        wr.close();
        rd.close();

        return "??";
    }

    private void login(final URL url) throws IOException {
        Proxy proxy = Proxy.NO_PROXY;
        if (Boolean.parseBoolean(System.getProperty("proxySet"))) {
            final String host = System.getProperty("proxyHost");
            final int port = Integer.parseInt(System.getProperty("proxyPort"));
            final InetSocketAddress addr = new InetSocketAddress(host, port);
            proxy = new Proxy(Proxy.Type.HTTP, addr);
        }
        URLConnection conn;

        conn = url.openConnection(proxy);

        // Get the response
        final BufferedReader rd = new BufferedReader(new InputStreamReader(
                conn.getInputStream()));
        String line;
        while ((line = rd.readLine()) != null) {
            // Process line...
            ErlLogger.debug(line);
        }
        rd.close();
    }

}
