package org.erlide.ui.internal;

import java.text.CharacterIterator;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

public class DocumentCharacterIterator implements CharacterIterator,
        CharSequence, Cloneable {

    private int fIndex = -1;

    private final IDocument fDocument;

    private final int fFirst;

    private final int fLast;

    private void invariant() {
        Assert.isTrue(fIndex >= fFirst);
        Assert.isTrue(fIndex <= fLast);
    }

    /**
     * Creates an iterator for the entire document.
     * 
     * @param document
     *            the document backing this iterator
     */
    public DocumentCharacterIterator(final IDocument document) {
        this(document, 0);
    }

    /**
     * Creates an iterator, starting at offset <code>first</code>.
     * 
     * @param document
     *            the document backing this iterator
     * @param first
     *            the first character to consider
     * @throws IllegalArgumentException
     *             if the indices are out of bounds
     */
    public DocumentCharacterIterator(final IDocument document, final int first)
            throws IllegalArgumentException {
        this(document, first, document.getLength());
    }

    /**
     * Creates an iterator for the document contents from <code>first</code>
     * (inclusive) to <code>last</code> (exclusive).
     * 
     * @param document
     *            the document backing this iterator
     * @param first
     *            the first character to consider
     * @param last
     *            the last character index to consider
     * @throws IllegalArgumentException
     *             if the indices are out of bounds
     */
    public DocumentCharacterIterator(final IDocument document, final int first,
            final int last) throws IllegalArgumentException {
        if (document == null) {
            throw new IllegalArgumentException("document can't be null");
        }
        if (first < 0 || first > last) {
            throw new IllegalArgumentException(
                    "iterating outside document bounds");
        }
        if (last > document.getLength()) {
            throw new IllegalArgumentException(
                    "iterating outside document bounds");
        }
        fDocument = document;
        fFirst = first;
        fLast = last;
        fIndex = first;
        invariant();
    }

    /*
     * @see java.text.CharacterIterator#first()
     */
    @Override
    public char first() {
        return setIndex(getBeginIndex());
    }

    /*
     * @see java.text.CharacterIterator#last()
     */
    @Override
    public char last() {
        if (fFirst == fLast) {
            return setIndex(getEndIndex());
        }
        return setIndex(getEndIndex() - 1);
    }

    /*
     * @see java.text.CharacterIterator#current()
     */
    @Override
    public char current() {
        if (fIndex >= fFirst && fIndex < fLast) {
            try {
                return fDocument.getChar(fIndex);
            } catch (final BadLocationException e) {
                // ignore
            }
        }
        return DONE;
    }

    /*
     * @see java.text.CharacterIterator#next()
     */
    @Override
    public char next() {
        return setIndex(Math.min(fIndex + 1, getEndIndex()));
    }

    /*
     * @see java.text.CharacterIterator#previous()
     */
    @Override
    public char previous() {
        if (fIndex > getBeginIndex()) {
            return setIndex(fIndex - 1);
        }
        return DONE;
    }

    /*
     * @see java.text.CharacterIterator#setIndex(int)
     */
    @Override
    public char setIndex(final int position) {
        if (position >= getBeginIndex() && position <= getEndIndex()) {
            fIndex = position;
        } else {
            throw new IllegalArgumentException();
        }

        invariant();
        return current();
    }

    /*
     * @see java.text.CharacterIterator#getBeginIndex()
     */
    @Override
    public int getBeginIndex() {
        return fFirst;
    }

    /*
     * @see java.text.CharacterIterator#getEndIndex()
     */
    @Override
    public int getEndIndex() {
        return fLast;
    }

    /*
     * @see java.text.CharacterIterator#getIndex()
     */
    @Override
    public int getIndex() {
        return fIndex;
    }

    /*
     * @see java.text.CharacterIterator#clone()
     */
    @Override
    public Object clone() {
        try {
            return super.clone();
        } catch (final CloneNotSupportedException e) {
            throw new InternalError();
        }
    }

    /*
     * @see java.lang.CharSequence#length()
     */
    @Override
    public int length() {
        return getEndIndex() - getBeginIndex();
    }

    /**
     * {@inheritDoc}
     * <p>
     * Note that, if the document is modified concurrently, this method may
     * return {@link CharacterIterator#DONE} if a {@link BadLocationException}
     * was thrown when accessing the backing document.
     * </p>
     * 
     * @param index
     *            {@inheritDoc}
     * @return {@inheritDoc}
     */
    @Override
    public char charAt(final int index) {
        if (index >= 0 && index < length()) {
            try {
                return fDocument.getChar(getBeginIndex() + index);
            } catch (final BadLocationException e) {
                // ignore and return DONE
                return DONE;
            }
        }
        throw new IndexOutOfBoundsException();
    }

    /*
     * @see java.lang.CharSequence#subSequence(int, int)
     */
    @Override
    public CharSequence subSequence(final int start, final int end) {
        if (start < 0) {
            throw new IndexOutOfBoundsException();
        }
        if (end < start) {
            throw new IndexOutOfBoundsException();
        }
        if (end > length()) {
            throw new IndexOutOfBoundsException();
        }
        return new DocumentCharacterIterator(fDocument,
                getBeginIndex() + start, getBeginIndex() + end);
    }
}
