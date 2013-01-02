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

import com.ericsson.otp.erlang.OtpMbox;

public interface IErlRuntime extends IRpcSite {

    boolean isAvailable();

    String getNodeName();

    void remoteStatus(final String node, final boolean up, final Object info);

    OtpMbox createMbox(String string);

    OtpMbox createMbox();

    void stop();

}
