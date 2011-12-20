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
package org.erlide.core.internal.model.erlang;

import org.erlide.core.model.erlang.ISourceRange;

/**
 * @see ISourceRange
 */
public class SourceRange implements ISourceRange {

    private final int fOffset;

    private final int fLength;

    public SourceRange(final int offset, final int length) {
        fOffset = offset;
        fLength = length;
    }

    /**
     * @see ISourceRange
     */
    @Override
    public int getLength() {
        return fLength;
    }

    /**
     * @see ISourceRange
     */
    @Override
    public int getOffset() {
        return fOffset;
    }

    @Override
    public String toString() {
        final StringBuilder buffer = new StringBuilder();
        buffer.append("[offset="); //$NON-NLS-1$
        buffer.append(fOffset);
        buffer.append(", length="); //$NON-NLS-1$
        buffer.append(fLength);
        buffer.append(']');
        return buffer.toString();
    }

    @Override
    public boolean hasPosition(final int position) {
        return position >= fOffset && position <= fOffset + fLength;
    }
}
