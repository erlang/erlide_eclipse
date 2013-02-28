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
package org.erlide.runtime;

import org.erlide.runtime.shell.IBackendShell;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public interface IErlRuntime {

    String getName();

    boolean isDistributed();

    RuntimeData getRuntimeData();

    boolean isAvailable();

    boolean isStopped();

    String getNodeName();

    void start();

    void stop();

    void connect();

    OtpMbox createMbox(String string);

    OtpMbox createMbox();

    OtpErlangPid getEventPid();

    OtpMbox getEventMbox();

    IRpcSite getRpcSite();

    void restart();

    void addListener(IRuntimeStateListener listener);

    IBackendShell getShell(final String id);

}
