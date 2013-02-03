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

import java.util.List;

import org.erlide.backend.console.IoRequest.IoRequestKind;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IBackendShell {

    void close();

    void send(final String string);

    String getId();

    void input(String s);

    void add(final OtpErlangObject msg);

    void add(final String text, final IoRequestKind kind);

    IoRequest findAtPos(final int thePos);

    List<IoRequest> getAllFrom(final OtpErlangPid sender);

    void add(final List<OtpErlangObject> msgs);

    void dispose();

    void addListener(final BackendShellListener listener);

    void removeListener(final BackendShellListener listener);

    int getTextLength();

    String getText();

}
