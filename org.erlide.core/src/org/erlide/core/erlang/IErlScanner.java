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

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.reconciler.DirtyRegion;

public interface IErlScanner {

	ErlToken getTokenAt(int offset);

	ErlToken[] getTokensAround(int offset);

	void modifyText(IDocument doc, DirtyRegion dirtyRegion);

	ErlToken[] getTokens();

	TokenWindow getTokenWindow(int offset, int window);

}
