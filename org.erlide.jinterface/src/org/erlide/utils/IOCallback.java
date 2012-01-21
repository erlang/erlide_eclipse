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
package org.erlide.utils;

import java.util.Collection;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IOCallback {

    OtpErlangObject putChars(OtpErlangPid from, IOEncoding encoding,
            OtpErlangObject chars);

    OtpErlangObject putChars(OtpErlangPid from, IOEncoding latin1,
            String module, String function, Collection<OtpErlangObject> args);

    OtpErlangObject getUntil(IOEncoding latin1, OtpErlangObject otpErlangObject);

    OtpErlangObject getUntil(IOEncoding latin1,
            OtpErlangObject otpErlangObject, long n);

    OtpErlangObject getUntil(IOEncoding valueOf,
            OtpErlangObject otpErlangObject, String m, String f,
            Collection<OtpErlangObject> a);

    OtpErlangObject getOpts();

    OtpErlangObject setOpts(Collection<OtpErlangObject> opts);

}
