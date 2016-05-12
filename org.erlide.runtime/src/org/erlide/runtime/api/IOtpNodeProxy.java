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

import java.util.concurrent.Executor;

import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.IDisposable;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.util.concurrent.Service.Listener;

public interface IOtpNodeProxy extends IDisposable {

    void ensureRunning();

    boolean isRunning();

    Process getProcess();

    String getNodeName();

    RuntimeVersion getVersion();

    String getOtpHome();

    IOtpRpc getOtpRpc();

    OtpMbox createMbox(String string);

    OtpMbox createMbox();

    OtpErlangPid getEventPid();

    void registerEventListener(Object handler);

    void addRuntimeListener(Listener listener, Executor executor);

}
