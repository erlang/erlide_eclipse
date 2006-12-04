/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.io.InputStream;
import java.io.OutputStream;

public class ErtsConsole {

	private OutputStream stdin;

	private InputStream stdout;

	private InputStream stderr;

	public ErtsConsole(OutputStream in, InputStream out, InputStream err) {
		stdin = in;
		stdout = out;
		stderr = err;
	}

	public OutputStream getStdin() {
		return stdin;
	}

	public InputStream getStdout() {
		return stdout;
	}

	public InputStream getStderr() {
		return stderr;
	}
}
