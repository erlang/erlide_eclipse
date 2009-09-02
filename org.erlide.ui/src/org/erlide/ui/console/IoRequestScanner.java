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
package org.erlide.ui.console;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IToken;
import org.erlide.jinterface.backend.BackendShell;
import org.erlide.jinterface.backend.console.IoRequest;

public class IoRequestScanner implements IPartitionTokenScanner {

	private final BackendShell model;
	private int docOffset;
	private int docLength;
	private int crtOffset;
	private int crtLength;

	public IoRequestScanner(BackendShell model) {
		Assert.isNotNull(model);
		this.model = model;
	}

	public void setPartialRange(IDocument document, int offset, int length,
			String contentType, int partitionOffset) {
		docOffset = offset;
		docLength = length;
		IoRequest req = model.findAtPos(docOffset);
		if (req != null) {
			crtOffset = req.getStart();
		} else {
			crtOffset = -1;
		}
		crtLength = 0;
	}

	public int getTokenLength() {
		if (crtOffset + crtLength > docLength) {
			return crtLength - (docLength - crtOffset);
		}
		return crtLength;
	}

	public int getTokenOffset() {
		return crtOffset;
	}

	public IToken nextToken() {
		IoRequest req;
		crtOffset = crtOffset + crtLength;
		if (crtOffset > docOffset + docLength) {
			return new IoRequestToken(null);
		}
		req = model.findAtPos(crtOffset);
		if (req != null) {
			crtLength = req.getLength();
		}
		IoRequestToken token = new IoRequestToken(req);
		return token;
	}

	public void setRange(IDocument document, int offset, int length) {
		docOffset = offset;
		docLength = length;
		IoRequest req = model.findAtPos(docOffset);
		if (req != null) {
			crtOffset = req.getStart();
		} else {
			crtOffset = -1;
		}
		crtLength = 0;
	}

	private class IoRequestToken implements IToken {

		private final String data;

		public IoRequestToken(IoRequest req) {
			if (req == null) {
				data = null;
			} else {
				this.data = req.getKind().name();
			}
		}

		public Object getData() {
			return data;
		}

		public boolean isEOF() {
			return data == null;
		}

		public boolean isOther() {
			return true;
		}

		public boolean isUndefined() {
			return false;
		}

		public boolean isWhitespace() {
			return false;
		}

		@Override
		public String toString() {
			return "TOK:" + data;
		}
	}
}
