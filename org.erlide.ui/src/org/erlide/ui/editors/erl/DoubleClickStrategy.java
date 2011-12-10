/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.source.ICharacterPairMatcher;

/**
 * The double click strategy for the editor
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class DoubleClickStrategy implements ITextDoubleClickStrategy {

    /**
     * The text container
     */
    protected ITextViewer fText;

    protected ICharacterPairMatcher fPairMatcher;

    public DoubleClickStrategy(final ICharacterPairMatcher matcher) {
        fPairMatcher = matcher;
    }

    /**
     * If something is doubleclicking
     * 
     * @see org.eclipse.jface.text.ITextDoubleClickStrategy#doubleClicked(org.eclipse.jface.text.ITextViewer)
     */
    @Override
    public void doubleClicked(final ITextViewer part) {
        final int offset = part.getSelectedRange().x;

        if (offset < 0) {
            return;
        }

        final IDocument document = part.getDocument();
        fText = part;

        final IRegion region = fPairMatcher.match(document, offset);
        if (region != null && region.getLength() >= 2) {
            part.setSelectedRange(region.getOffset(), region.getLength());
        } else {
            if (!selectComment(offset)) {
                selectWord(offset);
            }
        }
    }

    /**
     * if a t_comment is selected
     * 
     * @param caretPos
     * @return
     */
    protected boolean selectComment(final int caretPos) {
        final IDocument doc = fText.getDocument();
        final int startPos;
        final int endPos;

        try {
            int pos = caretPos;
            char c = ' ';

            while (pos >= 0) {
                c = doc.getChar(pos);
                if (c == '\\') {
                    pos -= 2;
                    continue;
                }
                if (c == Character.LINE_SEPARATOR
                        || c == Character.DIRECTIONALITY_PARAGRAPH_SEPARATOR
                        || c == '\"') {
                    break;
                }
                --pos;
            }

            if (c != '\"') {
                return false;
            }

            startPos = pos;

            pos = caretPos;
            final int length = doc.getLength();
            c = ' ';

            while (pos < length) {
                c = doc.getChar(pos);
                if (c == Character.LINE_SEPARATOR
                        || c == Character.DIRECTIONALITY_PARAGRAPH_SEPARATOR
                        || c == '\"') {
                    break;
                }
                ++pos;
            }
            if (c != '\"') {
                return false;
            }

            endPos = pos;

            final int offset = startPos + 1;
            final int len = endPos - offset;
            fText.setSelectedRange(offset, len);
            return true;
        } catch (final BadLocationException x) {
            // ignorable problem.
        }

        return false;
    }

    /**
     * Select a word
     * 
     * @param caretPos
     * @return
     */
    protected boolean selectWord(final int caretPos) {

        final IDocument doc = fText.getDocument();
        final int startPos;
        final int endPos;

        try {

            int pos = caretPos;
            char c;

            while (pos >= 0) {
                c = doc.getChar(pos);
                if (!Character.isJavaIdentifierPart(c)) {
                    break;
                }
                --pos;
            }

            startPos = pos;

            pos = caretPos;
            final int length = doc.getLength();

            while (pos < length) {
                c = doc.getChar(pos);
                if (!Character.isJavaIdentifierPart(c)) {
                    break;
                }
                ++pos;
            }

            endPos = pos;
            selectRange(startPos, endPos);
            return true;

        } catch (final BadLocationException x) {
            // ignorable error
        }

        return false;
    }

    /**
     * select a range of values
     * 
     * @param startPos
     *            the starting pos
     * @param stopPos
     *            the ending pos
     */
    private void selectRange(final int startPos, final int stopPos) {
        final int offset = startPos + 1;
        final int length = stopPos - offset;
        fText.setSelectedRange(offset, length);
    }
}
