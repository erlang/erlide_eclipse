/*******************************************************************************
 * Copyright (c) 2016 Red Hat Inc. and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *  Mickael Istria (Red Hat Inc.) - initial implementation
 *******************************************************************************/
package org.eclipse.languageserver;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Abstraction of a connection which we can start/stop and connect to via streams.
 * It's typically used to wrap startup of language servers and to retrieve their
 * streams.
 * There most likely an existing Java class already taking care of this somewhere
 * in a popular API. In such case, we should consider getting read of this one and
 * use a more popular similar interface.
 * Note that in the context of Eclipse, the ILaunch might be such interface but I'm
 * not sure we want to bind to org.eclipse.debug from this Language Server bindings.
 */
public interface StreamConnectionProvider {

    public void start() throws IOException;

    public InputStream getInputStream();

    public OutputStream getOutputStream();

    public void stop();

}
