/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.console;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.ITextStore;
import org.erlide.runtime.backend.console.ErlConsoleModel;
import org.erlide.runtime.backend.console.IoRequest;

public class IoRequestStore implements ITextStore {

	private final ErlConsoleModel model;

	public IoRequestStore(ErlConsoleModel model) {
		Assert.isNotNull(model);
		this.model = model;
	}

	public char get(int offset) {
		IoRequest req = model.findAtPos(offset);
		String msg = req.getMessage();
		return msg.charAt(offset - req.getStart());
	}

	public String get(int offset, int length) {
		IoRequest req = model.findAtPos(offset);
		if (req == null) {
			return "";
		}
		String msg = req.getMessage();
		int start = offset - req.getStart();
		int reqLen = req.getLength();
		if (length <= reqLen) {
			return msg.substring(start, start + length);
		} else {
			return msg.substring(start) + get(offset + reqLen, length - reqLen);
		}
	}

	public int getLength() {
		return model.getTextLength();
	}

	public void replace(int offset, int length, String text) {
	}

	public void set(String text) {
	}

}
