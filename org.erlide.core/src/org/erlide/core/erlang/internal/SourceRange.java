/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.ISourceRange;

/**
 * @see ISourceRange
 */
public class SourceRange implements ISourceRange {

	private int fOffset;

	private int fLength;

	public SourceRange(int offset, int length) {
		this.fOffset = offset;
		this.fLength = length;
	}

	/**
	 * @see ISourceRange
	 */
	public int getLength() {
		return this.fLength;
	}

	/**
	 * @see ISourceRange
	 */
	public int getOffset() {
		return this.fOffset;
	}

	@Override
	public String toString() {
		final StringBuffer buffer = new StringBuffer();
		buffer.append("[offset="); //$NON-NLS-1$
		buffer.append(this.fOffset);
		buffer.append(", length="); //$NON-NLS-1$
		buffer.append(this.fLength);
		buffer.append("]"); //$NON-NLS-1$
		return buffer.toString();
	}

	public boolean hasPosition(int position) {
		return position >= fOffset && position < fOffset + fLength;
	}
}
