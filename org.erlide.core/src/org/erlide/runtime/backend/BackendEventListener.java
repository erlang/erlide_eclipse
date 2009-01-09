/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface BackendEventListener {

	/*
	 * This executes in a separate thread! Any UI work must be done with a
	 * Display.asyncExec or syncExec
	 */
	void eventReceived(OtpErlangObject event);
}