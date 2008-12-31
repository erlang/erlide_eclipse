/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend.internal;

import org.eclipse.debug.core.ILaunch;
import org.erlide.runtime.backend.RuntimeInfo;
import org.erlide.runtime.backend.console.BackendShellManager;
import org.erlide.runtime.backend.exceptions.BackendException;

/**
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 */
public class StandaloneBackend extends AbstractBackend {

	public StandaloneBackend(RuntimeInfo info) throws BackendException {
		super(info);
	}

	@Override
	public void connect() {
		doConnect(getName());
	}

	public void stop() {
	}

	@Override
	public void dispose() {
		super.dispose();
	}

	@Override
	public void initializeRuntime(ILaunch launch) {
		fShellManager = new BackendShellManager(this);
	}

}
