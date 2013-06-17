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
package org.erlide.runtime.api;

import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.IDisposable;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public interface IErlRuntime extends IBeamProcess, IDisposable {

    String getNodeName();

    IRpcSite getRpcSite();

    RuntimeData getRuntimeData();

    boolean isAvailable();

    // TODO naming
    void connect();

    OtpMbox createMbox(String string);

    OtpMbox createMbox();

    OtpErlangPid getEventPid();

    OtpMbox getEventMbox();

    IBackendShell getShell(final String id);

    void addListener(IRuntimeStateListener listener);

    ErlSystemStatus getSystemStatus();

    void setSystemStatus(ErlSystemStatus msg);

    void registerEventHandler(Object handler);
}
