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

    @Override
    public char getChar(final int offset) {
        return text.charAt(offset);
    }

    @Override
    public int getLength() {
        return text.length();
    }

    @Override
    public String get() {
        return new String(text);
    }

    @Override
    public String get(final int offset, final int length) {
        return text.substring(offset, offset + length);
    }

    @Override
    public void set(final String text) {
        this.text = new String(text);
    }

    @Override
    public void replace(final int offset, final int length, final String theText) {
        text = text.substring(0, offset) + theText
                + text.substring(offset + length);
    }

    @Override
    public void addDocumentListener(final IDocumentListener listener) {
        // defining interface method
    }

    @Override
    public void removeDocumentListener(final IDocumentListener listener) {
        // defining interface method
    }

    @Override
    public void addPrenotifiedDocumentListener(
            final IDocumentListener documentAdapter) {
        // defining interface method
    }

    @Override
    public void removePrenotifiedDocumentListener(
            final IDocumentListener documentAdapter) {
        // defining interface method
    }

    @Override
    public void addPositionCategory(final String category) {
        // defining interface method
    }

    @Override
    public void removePositionCategory(final String category) {
        // defining interface method
    }

    @Override
    public String[] getPositionCategories() {
        // defining interface method
        return null;
    }

    @Override
    public boolean containsPositionCategory(final String category) {
        // defining interface method
        return false;
    }

    @Override
    public void addPosition(final Position position) {
        // defining interface method
    }

    @Override
    public void removePosition(final Position position) {
        // defining interface method
    }

    @Override
    public void addPosition(final String category, final Position position) {
        // defining interface method
    }

    @Override
    public void removePosition(final String category, final Position position) {
        // defining interface method
    }

    @Override
    public Position[] getPositions(final String category) {
        // defining interface method
        return null;
    }

    @Override
    public boolean containsPosition(final String category, final int offset,
            final int length) {
        // defining interface method
        return false;
    }

    @Override
    public int computeIndexInCategory(final String category, final int offset) {
        // defining interface method
        return 0;
    }

    @Override
    public void addPositionUpdater(final IPositionUpdater updater) {
        // defining interface method
    }

    @Override
    public void removePositionUpdater(final IPositionUpdater updater) {
        // defining interface method
    }

    @Override
    public void insertPositionUpdater(final IPositionUpdater updater,
            final int index) {
        // defining interface method
    }

    @Override
    public IPositionUpdater[] getPositionUpdaters() {
        // defining interface method
        return null;
    }

    @Override
    public String[] getLegalContentTypes() {
        // defining interface method
        return null;
    }

    @Override
    public String getContentType(final int offset) {
        // defining interface method
        return null;
    }

    @Override
    public ITypedRegion getPartition(final int offset) {
        // defining interface method
        return null;
    }

    @Override
    public ITypedRegion[] computePartitioning(final int offset, final int length) {
        // defining interface method
        return null;
    }

    @Override
    public void addDocumentPartitioningListener(
            final IDocumentPartitioningListener listener) {
        // defining interface method
    }

    @Override
    public void removeDocumentPartitioningListener(
            final IDocumentPartitioningListener listener) {
        // defining interface method
    }

    @Override
    public void setDocumentPartitioner(final IDocumentPartitioner partitioner) {
        // defining interface method
    }

    @Override
    public IDocumentPartitioner getDocumentPartitioner() {
        // defining interface method
        return null;
    }

    @Override
    public int getLineLength(final int line) {
        // defining interface method
        return 0;
    }

    @Override
    public int getLineOfOffset(final int offset) {
        // defining interface method
        return 0;
    }

    @Override
    public int getLineOffset(final int line) {
        // defining interface method
        return 0;
    }

    @Override
    public IRegion getLineInformation(final int line) {
        // defining interface method
        return null;
    }

    @Override
    public IRegion getLineInformationOfOffset(final int offset) {
        // defining interface method
        return null;
    }

    @Override
    public int getNumberOfLines() {
        // defining interface method
        return 0;
    }

    @Override
    public int getNumberOfLines(final int offset, final int length) {
        // defining interface method
        return 0;
    }

    @Override
    public int computeNumberOfLines(final String theText) {
        // defining interface method
        return 0;
    }

    @Override
    public String[] getLegalLineDelimiters() {
        // defining interface method
        return null;
    }

    @Override
    public String getLineDelimiter(final int line) {
        // defining interface method
        return null;
    }

    /**
     * @see org.eclipse.jface.text.IDocument#search(int, java.lang.String,
     *      boolean, boolean, boolean)
     * @deprecated
     */
    @Override
    @Deprecated
    public int search(final int startOffset, final String findString,
            final boolean forwardSearch, final boolean caseSensitive,
            final boolean wholeWord) {
        // defining interface method
        return 0;
    }

}
