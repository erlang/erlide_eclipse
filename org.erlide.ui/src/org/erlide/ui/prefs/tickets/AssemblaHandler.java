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
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import org.erlide.ui.prefs.ProblemData;

public class AssemblaHandler extends TicketHandlerImpl {

    private static final String MYURL = "http://www.assembla.com/spaces/erlide/tickets/";
    // private static final String LOGINURL =
    // "https://www.assembla.com/users/login";

    private static final byte[] userHash = { (byte) 248, (byte) 254,
            (byte) 185, (byte) 157, (byte) 211, 11, 33, (byte) 206, (byte) 147,
            (byte) 211, (byte) 177, 10, 93, (byte) 209, (byte) 151, 59 };

    private boolean checkUser(final String user) {
        try {
            final MessageDigest algorithm = MessageDigest.getInstance("MD5");
            algorithm.reset();
            algorithm.update(user.getBytes());
            final byte[] messageDigest = algorithm.digest();
            return Arrays.equals(messageDigest, userHash);
        } catch (final NoSuchAlgorithmException nsae) {
        }
        return false;
    }

    @Override
    public String infoToMessage(final ProblemData info) {
        return "<ticket><summary>" + info.summary + "</summary></ticket>";
    }

    @Override
    public TicketStatus parseMessage(final String message) {
        final boolean ok = false;
        final int id = 0;

        final TicketStatus result = new TicketStatus(ok, id);
        return result;
    }

    @Override
    public URL getLoginURL(final String user, final String pass)
            throws MalformedURLException {
        if (checkUser(user)) {
            final String str = String.format(MYURL + "?user=%s&pass=%s", user,
                    pass);
            return new URL(str);
        }
        return null;
    }

}
