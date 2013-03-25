/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.util.concurrent.FutureCallback;

public interface IRpcCallback extends FutureCallback<OtpErlangObject> {

}
