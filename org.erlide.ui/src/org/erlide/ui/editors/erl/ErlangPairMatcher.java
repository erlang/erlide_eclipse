/*******************************************************************************
 * Copyright (c) 2006, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Christian Plesner Hansen (plesner@quenta.org) - initial API and implementation
 *     Vlad Dumitrescu - adapted to Erlang delimiters
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.jface.text.source.ICharacterPairMatcher;

/*
 * taken from eclipse 3.3 DefaultCharacterPairMatcher (which isn't in 3.2, unfortunately)
 */

public class ErlangPairMatcher implements ICharacterPairMatcher {

    private int fAnchor = -1;
    private final StringPairs fPairs;
    private final String fPartitioning;

    /**
     * Creates a new character pair matcher that matches the specified
     * characters within the specified partitioning. The specified list of
     * characters must have the form <blockquote>{ <i>start</i>, <i>end</i>,
     * <i>start</i>, <i>end</i>, ..., <i>start</i>, <i>end</i> }</blockquote>
     * For instance:
     * 
     * <pre>
     * char[] chars = new char[] {'(', ')', '{', '}', '[', ']'};
     * new SimpleCharacterPairMatcher(chars, ...);
     * </pre>
     * 
     * @param chars
     *            a list of characters
     * @param partitioning
     *            the partitioning to match within
     */
    public ErlangPairMatcher(final String[] strings, final String partitioning) {
        Assert.isLegal(strings.length % 2 == 0);
        Assert.isNotNull(partitioning);
        fPairs = new StringPairs(strings);
        fPartitioning = partitioning;
    }

    /**
     * Creates a new character pair matcher that matches characters within the
     * default partitioning. The specified list of characters must have the form
     * <blockquote>{ <i>start</i>, <i>end</i>, <i>start</i>, <i>end</i>, ...,
     * <i>start</i>, <i>end</i> }</blockquote> For instance:
     * 
     * <pre>
     * char[] chars = new char[] { '(', ')', '{', '}', '[', ']' };
     * new SimpleCharacterPairMatcher(chars);
     * </pre>
     * 
     * @param chars
     *            a list of characters
     */
    public ErlangPairMatcher(final String[] strings) {
        this(strings, IDocumentExtension3.DEFAULT_PARTITIONING);
    }

    /* @see ICharacterPairMatcher#match(IDocument, int) */
    @Override
    public IRegion match(final IDocument doc, final int offset) {
        if (doc == null || offset < 0 || offset > doc.getLength()) {
            return null;
        }
        try {
            return performMatch(doc, offset);
        } catch (final BadLocationException ble) {
            return null;
        }
    }

    /*
     * Performs the actual work of matching for #match(IDocument, int).
     */
    private IRegion performMatch(final IDocument doc, final int offset)
            throws BadLocationException {
        if (offset <= 0) {
            return null;
        }
        String prevString = doc.get(offset - 1, 1);
        if (!fPairs.contains(prevString)) {
            if (offset == 1) {
                return null;
            }
            prevString = doc.get(offset - 2, 2);
            if (!fPairs.contains(prevString)) {
                return null;
            }
        }
        final boolean isForward = fPairs.isStartString(prevString);
        fAnchor = isForward ? ICharacterPairMatcher.LEFT
                : ICharacterPairMatcher.RIGHT;
        final int searchStartPosition = isForward ? offset : offset
                - (prevString.length() + 1);
        final int adjustedOffset = isForward ? offset - 1 : offset;
        final String partition = TextUtilities.getContentType(doc,
                fPartitioning, adjustedOffset, false);
        final DocumentPartitionAccessor partDoc = new DocumentPartitionAccessor(
                doc, fPartitioning, partition);
        final int endOffset = findMatchingPeer(partDoc, prevString,
                fPairs.getMatching(prevString), isForward,
                isForward ? doc.getLength() - prevString.length() + 1 : -1,
                searchStartPosition);
        if (endOffset == -1) {
            return null;
        }
        final int adjustedEndOffset = isForward ? endOffset + 1 : endOffset;
        if (adjustedEndOffset == adjustedOffset) {
            return null;
        }
        return new Region(Math.min(adjustedOffset, adjustedEndOffset),
                Math.abs(adjustedEndOffset - adjustedOffset));
    }

    /**
     * Searches <code>doc</code> for the specified end string, <code>end</code>.
     * 
     * @param doc
     *            the document to search
     * @param start
     *            the opening matching string
     * @param end
     *            the end string to search for
     * @param searchForward
     *            search forwards or backwards?
     * @param boundary
     *            a boundary at which the search should stop
     * @param startPos
     *            the start offset
     * @return the index of the end character if it was found, otherwise -1
     * @throws BadLocationException
     */
    private int findMatchingPeer(final DocumentPartitionAccessor doc,
            final String start, final String end, final boolean searchForward,
            final int boundary, final int startPos) throws BadLocationException {
        int pos = startPos;
        final int length = start.length();
        while (pos != boundary) {
            final String s = doc.get(pos, length);
            if (doc.isMatch(pos, end)) {
                return pos;
            } else if (s.equals(start) && doc.inPartition(pos)) {
                pos = findMatchingPeer(doc, start, end, searchForward,
                        boundary, doc.getNextPosition(pos, searchForward));
                if (pos == -1) {
                    return -1;
                }
            }
            pos = doc.getNextPosition(pos, searchForward);
        }
        return -1;
    }

    /* @see ICharacterPairMatcher#getAnchor() */
    @Override
    public int getAnchor() {
        return fAnchor;
    }

    /* @see ICharacterPairMatcher#dispose() */
    @Override
    public void dispose() {
    }

    /* @see ICharacterPairMatcher#clear() */
    @Override
    public void clear() {
        fAnchor = -1;
    }

    /**
     * Utility class that wraps a document and gives access to partitioning
     * information. A document is tied to a particular partition and, when
     * considering whether or not a position is a valid match, only considers
     * position within its partition.
     */
    private static class DocumentPartitionAccessor {

        private final IDocument fDocument;
        private final String fPartitioning, fPartition;
        private ITypedRegion fCachedPartition;

        /**
         * Creates a new partitioned document for the specified document.
         * 
         * @param doc
         *            the document to wrap
         * @param partitioning
         *            the partitioning used
         * @param partition
         *            the partition managed by this document
         */
        public DocumentPartitionAccessor(final IDocument doc,
                final String partitioning, final String partition) {
            fDocument = doc;
            fPartitioning = partitioning;
            fPartition = partition;
        }

        /**
         * Returns the character at the specified position in this document.
         * 
         * @param pos
         *            an offset within this document
         * @param length
         *            a length of string to get
         * @return the character at the offset
         * @throws BadLocationException
         */
        public String get(final int pos, final int length)
                throws BadLocationException {
            return fDocument.get(pos, length);
        }

        /**
         * Returns true if the character at the specified position is a valid
         * match for the specified end character. To be a valid match, it must
         * be in the appropriate partition and equal to the end character.
         * 
         * @param pos
         *            an offset within this document
         * @param end
         *            the end character to match against
         * @return true exactly if the position represents a valid match
         * @throws BadLocationException
         */
        public boolean isMatch(final int pos, final String end)
                throws BadLocationException {
            return get(pos, end.length()).equals(end) && inPartition(pos);
        }

        /**
         * Returns true if the specified offset is within the partition managed
         * by this document.
         * 
         * @param pos
         *            an offset within this document
         * @return true if the offset is within this document's partition
         */
        public boolean inPartition(final int pos) {
            final ITypedRegion partition = getPartition(pos);
            return partition != null && partition.getType().equals(fPartition);
        }

        /**
         * Returns the next position to query in the search. The position is not
         * guaranteed to be in this document's partition.
         * 
         * @param pos
         *            an offset within the document
         * @param searchForward
         *            the direction of the search
         * @return the next position to query
         */
        public int getNextPosition(final int pos, final boolean searchForward) {
            final ITypedRegion partition = getPartition(pos);
            if (partition == null) {
                return simpleIncrement(pos, searchForward);
            }
            if (fPartition.equals(partition.getType())) {
                return simpleIncrement(pos, searchForward);
            }
            if (searchForward) {
                final int end = partition.getOffset() + partition.getLength();
                if (pos < end) {
                    return end;
                }
            } else {
                final int offset = partition.getOffset();
                if (pos > offset) {
                    return offset - 1;
                }
            }
            return simpleIncrement(pos, searchForward);
        }

        private int simpleIncrement(final int pos, final boolean searchForward) {
            return pos + (searchForward ? 1 : -1);
        }

        /**
         * Returns partition information about the region containing the
         * specified position.
         * 
         * @param pos
         *            a position within this document.
         * @return positioning information about the region containing the
         *         position
         */
        private ITypedRegion getPartition(final int pos) {
            if (fCachedPartition == null || !contains(fCachedPartition, pos)) {
                Assert.isTrue(pos >= 0 && pos <= fDocument.getLength());
                try {
                    fCachedPartition = TextUtilities.getPartition(fDocument,
                            fPartitioning, pos, false);
                } catch (final BadLocationException e) {
                    fCachedPartition = null;
                }
            }
            return fCachedPartition;
        }

        private static boolean contains(final IRegion region, final int pos) {
            final int offset = region.getOffset();
            return offset <= pos && pos < offset + region.getLength();
        }

    }

    /**
     * Utility class that encapsulates access to matching character pairs.
     */
    private static class StringPairs {

        private final String[] fPairs;

        public StringPairs(final String[] strings) {
            fPairs = strings;
        }

        /**
         * Returns true if the specified character pair occurs in one of the
         * character pairs.
         * 
         * @param c
         *            a character
         * @return true exactly if the character occurs in one of the pairs
         */
        public boolean contains(final String s) {
            return getAllCharacters().contains(s);
        }

        private Set<String> fStringsCache = null;

        /**
         * @return A set containing all characters occurring in character pairs.
         */
        private Set<String> getAllCharacters() {
            if (fStringsCache == null) {
                final Set<String> set = new HashSet<String>();
                for (int i = 0; i < fPairs.length; i++) {
                    set.add(fPairs[i]);
                }
                fStringsCache = set;
            }
            return fStringsCache;
        }

        /**
         * Returns true if the specified character opens a character pair when
         * scanning in the specified direction.
         * 
         * @param c
         *            a character
         * @param searchForward
         *            the direction of the search
         * @return whether or not the character opens a character pair
         */
        public boolean isOpeningString(final String s,
                final boolean searchForward) {
            for (int i = 0; i < fPairs.length; i += 2) {
                if (searchForward && getStartString(i).equals(s)) {
                    return true;
                } else if (!searchForward && getEndString(i).equals(s)) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Returns true of the specified string is a start string.
         * 
         * @param s
         *            a string
         * @return true exactly if the string is a start string
         */
        public boolean isStartString(final String s) {
            return isOpeningString(s, true);
        }

        /**
         * Returns the matching character for the specified character.
         * 
         * @param s
         *            a string occurring in a string pair
         * @return the matching string
         */
        public String getMatching(final String s) {
            for (int i = 0; i < fPairs.length; i += 2) {
                if (getStartString(i).equals(s)) {
                    return getEndString(i);
                } else if (getEndString(i).equals(s)) {
                    return getStartString(i);
                }
            }
            Assert.isTrue(false);
            return "";
        }

        private String getStartString(final int i) {
            return fPairs[i];
        }

        private String getEndString(final int i) {
            return fPairs[i + 1];
        }

    }
}
