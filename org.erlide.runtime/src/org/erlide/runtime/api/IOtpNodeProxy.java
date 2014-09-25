/*******************************************************************************
 * Copyright (c) 2009-2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.api;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.IDisposable;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public interface IOtpNodeProxy extends IDisposable {

    void startAndWait();

    boolean isRunning();

    Process getProcess();

    String getNodeName();

    RuntimeVersion getVersion();

    String getOtpHome();

    IOtpRpc getOtpRpc();

    OtpMbox createMbox(String string);

    OtpMbox createMbox();

    void setShutdownCallback(IShutdownCallback callback);

    OtpErlangPid getEventPid();

    void registerEventListener(Object handler);

}
