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
package org.erlide.core.erlang;

import java.util.Collection;

import org.erlide.jinterface.backend.IDisposable;

public interface IErlScanner extends IDisposable {

	// ErlToken[] getTokens();

	// the window is asymmetric, to allow finding number of arguments
	// TokenWindow getTokenWindow(int offset, int window);

	ErlToken getTokenAt(int offset);

	// ErlToken[] getTokensAround(int offset);

	// public void removeText(int offset, int length);
	// public void insertText(int offset, String text);

	public void replaceText(int offset, int removeLength, String newText);

	// void rescan(String fullText);

	Collection<IErlComment> getComments();

	public String getScannerModuleName();

}
