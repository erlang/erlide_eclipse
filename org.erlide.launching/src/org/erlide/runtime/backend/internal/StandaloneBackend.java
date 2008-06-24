/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend.internal;

import java.io.IOException;

import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IProcess;

public class StandaloneBackend extends AbstractBackend {

	@Override
	public void connect(final String cookie) {
		doConnect(getLabel(), cookie);
	}

	@Override
	public void sendToDefaultShell(final String msg) throws IOException {
		// TODO Auto-generated method stub

	}

	// @Override
	// public void sendToShell(final String str) {
	// // TODO Auto-generated method stub
	//
	// }

	@Override
	public void addStdListener(final IStreamListener dsp) {
		// TODO Auto-generated method stub

	}

	@Override
	public void initializeErts() {
		// TODO
	}

	@Override
	public void setErts(final IProcess process) {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.runtime.backend.internal.AbstractBackend#init_erlang()
	 */
	@Override
	public void initErlang() {
		// super.init_erlang();
	}

}
