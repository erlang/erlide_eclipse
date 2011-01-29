/*******************************************************************************
 * Copyright (c) 2000, 2009 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.tests;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.IDocumentPartitioningListener;
import org.eclipse.jface.text.IPositionUpdater;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;

/**
 * Minimal implementation of IDocument to apply text edit onto a string.
 */
public class StringDocument implements IDocument {

    private String text;

    public StringDocument(final String source) {
        text = source;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getChar(int)
     */
    public char getChar(final int offset) {
        return text.charAt(offset);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLength()
     */
    public int getLength() {
        return text.length();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#get()
     */
    public String get() {
        return new String(text);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#get(int, int)
     */
    public String get(final int offset, final int length) {
        return text.substring(offset, offset + length);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#set(java.lang.String)
     */
    public void set(final String text) {
        this.text = new String(text);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#replace(int, int, java.lang.String)
     */
    public void replace(final int offset, final int length, final String theText) {
        this.text = text.substring(0, offset) + theText
                + text.substring(offset + length);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#addDocumentListener(org.eclipse.jface
     * .text.IDocumentListener)
     */
    public void addDocumentListener(final IDocumentListener listener) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#removeDocumentListener(org.eclipse.jface
     * .text.IDocumentListener)
     */
    public void removeDocumentListener(final IDocumentListener listener) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#addPrenotifiedDocumentListener(org.eclipse
     * .jface.text.IDocumentListener)
     */
    public void addPrenotifiedDocumentListener(
            final IDocumentListener documentAdapter) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#removePrenotifiedDocumentListener(org
     * .eclipse.jface.text.IDocumentListener)
     */
    public void removePrenotifiedDocumentListener(
            final IDocumentListener documentAdapter) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#addPositionCategory(java.lang.String)
     */
    public void addPositionCategory(final String category) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#removePositionCategory(java.lang.String)
     */
    public void removePositionCategory(final String category) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getPositionCategories()
     */
    public String[] getPositionCategories() {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#containsPositionCategory(java.lang.String
     * )
     */
    public boolean containsPositionCategory(final String category) {
        // defining interface method
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#addPosition(org.eclipse.jface.text.Position
     * )
     */
    public void addPosition(final Position position) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#removePosition(org.eclipse.jface.text
     * .Position)
     */
    public void removePosition(final Position position) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#addPosition(java.lang.String,
     * org.eclipse.jface.text.Position)
     */
    public void addPosition(final String category, final Position position) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#removePosition(java.lang.String,
     * org.eclipse.jface.text.Position)
     */
    public void removePosition(final String category, final Position position) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getPositions(java.lang.String)
     */
    public Position[] getPositions(final String category) {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#containsPosition(java.lang.String,
     * int, int)
     */
    public boolean containsPosition(final String category, final int offset,
            final int length) {
        // defining interface method
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#computeIndexInCategory(java.lang.String,
     * int)
     */
    public int computeIndexInCategory(final String category, final int offset) {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#addPositionUpdater(org.eclipse.jface
     * .text.IPositionUpdater)
     */
    public void addPositionUpdater(final IPositionUpdater updater) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#removePositionUpdater(org.eclipse.jface
     * .text.IPositionUpdater)
     */
    public void removePositionUpdater(final IPositionUpdater updater) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#insertPositionUpdater(org.eclipse.jface
     * .text.IPositionUpdater, int)
     */
    public void insertPositionUpdater(final IPositionUpdater updater,
            final int index) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getPositionUpdaters()
     */
    public IPositionUpdater[] getPositionUpdaters() {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLegalContentTypes()
     */
    public String[] getLegalContentTypes() {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getContentType(int)
     */
    public String getContentType(final int offset) {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getPartition(int)
     */
    public ITypedRegion getPartition(final int offset) {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#computePartitioning(int, int)
     */
    public ITypedRegion[] computePartitioning(final int offset, final int length) {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#addDocumentPartitioningListener(org.
     * eclipse.jface.text.IDocumentPartitioningListener)
     */
    public void addDocumentPartitioningListener(
            final IDocumentPartitioningListener listener) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#removeDocumentPartitioningListener(org
     * .eclipse.jface.text.IDocumentPartitioningListener)
     */
    public void removeDocumentPartitioningListener(
            final IDocumentPartitioningListener listener) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#setDocumentPartitioner(org.eclipse.jface
     * .text.IDocumentPartitioner)
     */
    public void setDocumentPartitioner(final IDocumentPartitioner partitioner) {
        // defining interface method
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getDocumentPartitioner()
     */
    public IDocumentPartitioner getDocumentPartitioner() {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLineLength(int)
     */
    public int getLineLength(final int line) {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLineOfOffset(int)
     */
    public int getLineOfOffset(final int offset) {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLineOffset(int)
     */
    public int getLineOffset(final int line) {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLineInformation(int)
     */
    public IRegion getLineInformation(final int line) {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLineInformationOfOffset(int)
     */
    public IRegion getLineInformationOfOffset(final int offset) {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getNumberOfLines()
     */
    public int getNumberOfLines() {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getNumberOfLines(int, int)
     */
    public int getNumberOfLines(final int offset, final int length) {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.text.IDocument#computeNumberOfLines(java.lang.String)
     */
    public int computeNumberOfLines(final String theText) {
        // defining interface method
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLegalLineDelimiters()
     */
    public String[] getLegalLineDelimiters() {
        // defining interface method
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.text.IDocument#getLineDelimiter(int)
     */
    public String getLineDelimiter(final int line) {
        // defining interface method
        return null;
    }

    /**
     * @see org.eclipse.jface.text.IDocument#search(int, java.lang.String,
     *      boolean, boolean, boolean)
     * @deprecated
     */
    @Deprecated
    public int search(final int startOffset, final String findString,
            final boolean forwardSearch, final boolean caseSensitive,
            final boolean wholeWord) {
        // defining interface method
        return 0;
    }

}
