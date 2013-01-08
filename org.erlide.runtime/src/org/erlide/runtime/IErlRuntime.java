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

import org.erlide.runtime.runtimeinfo.RuntimeInfo;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public interface IErlRuntime {

    RuntimeData getRuntimeData();

    RuntimeInfo getRuntimeInfo();

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

}
