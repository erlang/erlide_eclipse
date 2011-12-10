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

/**
 * Specified Range, which has offset attributes.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class ErlRange extends Range implements IErlRange {
    protected int offset, length;

    /**
     * Constructor
     * 
     * @param startLine
     *            start line
     * @param startCol
     *            start column
     * @param endLine
     *            end line
     * @param endCol
     *            end column
     * @param offset
     *            selection's offset
     * @param length
     *            selection's length
     */
    public ErlRange(final int startLine, final int startCol, final int endLine,
            final int endCol, final int offset, final int length) {
        super(startLine, startCol, endLine, endCol);
        this.offset = offset;
        this.length = length;
    }

    /**
     * Constructor with a range and a document
     * 
     * @param range
     *            selection range
     * @param doc
     *            containing document
     */
    public ErlRange(final IRange range, final IDocument doc) {
        super(range.getStartLine(), range.getStartCol(), range.getEndLine(),
                range.getEndCol());

        try {
            offset = WranglerUtils.calculateOffsetFromPosition(startLine,
                    startCol, doc);
            length = WranglerUtils.calculateOffsetFromPosition(endLine, endCol,
                    doc) - offset + 1;
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }
    }

    @Override
    public int getLength() {
        return length;
    }

    @Override
    public int getOffset() {
        return offset;
    }

}
