/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.util;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

public class ErlRange extends Range implements IErlRange {
	protected int offset, length;

	public ErlRange(int startLine, int startCol, int endLine, int endCol,
			int offset, int length) {
		super(startLine, startCol, endLine, endCol);
		this.offset = offset;
		this.length = length;
	}

	public ErlRange(IRange range, IDocument doc) {
		super(range.getStartLine(), range.getStartCol(), range.getEndLine(),
				range.getEndCol());

		try {
			offset = WranglerUtils.calculateOffsetFromPosition(startLine,
					startCol, doc);
			length = WranglerUtils.calculateOffsetFromPosition(endLine, endCol,
					doc)
					- offset + 1;
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

	public int getLength() {
		return length;
	}

	public int getOffset() {
		return offset;
	}

}
