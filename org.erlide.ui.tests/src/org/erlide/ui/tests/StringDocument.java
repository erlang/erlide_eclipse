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

    public char getChar(final int offset) {
        return text.charAt(offset);
    }

    public int getLength() {
        return text.length();
    }

    public String get() {
        return new String(text);
    }

    public String get(final int offset, final int length) {
        return text.substring(offset, offset + length);
    }

    public void set(final String text) {
        this.text = new String(text);
    }

    public void replace(final int offset, final int length, final String theText) {
        text = text.substring(0, offset) + theText
                + text.substring(offset + length);
    }

    public void addDocumentListener(final IDocumentListener listener) {
        // defining interface method
    }

    public void removeDocumentListener(final IDocumentListener listener) {
        // defining interface method
    }

    public void addPrenotifiedDocumentListener(
            final IDocumentListener documentAdapter) {
        // defining interface method
    }

    public void removePrenotifiedDocumentListener(
            final IDocumentListener documentAdapter) {
        // defining interface method
    }

    public void addPositionCategory(final String category) {
        // defining interface method
    }

    public void removePositionCategory(final String category) {
        // defining interface method
    }

    public String[] getPositionCategories() {
        // defining interface method
        return null;
    }

    public boolean containsPositionCategory(final String category) {
        // defining interface method
        return false;
    }

    public void addPosition(final Position position) {
        // defining interface method
    }

    public void removePosition(final Position position) {
        // defining interface method
    }

    public void addPosition(final String category, final Position position) {
        // defining interface method
    }

    public void removePosition(final String category, final Position position) {
        // defining interface method
    }

    public Position[] getPositions(final String category) {
        // defining interface method
        return null;
    }

    public boolean containsPosition(final String category, final int offset,
            final int length) {
        // defining interface method
        return false;
    }

    public int computeIndexInCategory(final String category, final int offset) {
        // defining interface method
        return 0;
    }

    public void addPositionUpdater(final IPositionUpdater updater) {
        // defining interface method
    }

    public void removePositionUpdater(final IPositionUpdater updater) {
        // defining interface method
    }

    public void insertPositionUpdater(final IPositionUpdater updater,
            final int index) {
        // defining interface method
    }

    public IPositionUpdater[] getPositionUpdaters() {
        // defining interface method
        return null;
    }

    public String[] getLegalContentTypes() {
        // defining interface method
        return null;
    }

    public String getContentType(final int offset) {
        // defining interface method
        return null;
    }

    public ITypedRegion getPartition(final int offset) {
        // defining interface method
        return null;
    }

    public ITypedRegion[] computePartitioning(final int offset, final int length) {
        // defining interface method
        return null;
    }

    public void addDocumentPartitioningListener(
            final IDocumentPartitioningListener listener) {
        // defining interface method
    }

    public void removeDocumentPartitioningListener(
            final IDocumentPartitioningListener listener) {
        // defining interface method
    }

    public void setDocumentPartitioner(final IDocumentPartitioner partitioner) {
        // defining interface method
    }

    public IDocumentPartitioner getDocumentPartitioner() {
        // defining interface method
        return null;
    }

    public int getLineLength(final int line) {
        // defining interface method
        return 0;
    }

    public int getLineOfOffset(final int offset) {
        // defining interface method
        return 0;
    }

    public int getLineOffset(final int line) {
        // defining interface method
        return 0;
    }

    public IRegion getLineInformation(final int line) {
        // defining interface method
        return null;
    }

    public IRegion getLineInformationOfOffset(final int offset) {
        // defining interface method
        return null;
    }

    public int getNumberOfLines() {
        // defining interface method
        return 0;
    }

    public int getNumberOfLines(final int offset, final int length) {
        // defining interface method
        return 0;
    }

    public int computeNumberOfLines(final String theText) {
        // defining interface method
        return 0;
    }

    public String[] getLegalLineDelimiters() {
        // defining interface method
        return null;
    }

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
