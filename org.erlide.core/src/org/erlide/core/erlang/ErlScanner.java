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
package org.erlide.core.erlang;

import org.erlide.jinterface.backend.IDisposable;

import erlang.ErlideScanner;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IDisposable {
    private int refCount = 0;
    private final String scannerName;

    public ErlScanner(final String scannerName, final String initialText,
            final String path, final boolean useCaches) {
        this.scannerName = scannerName;
        ErlideScanner.initialScan(scannerName, path, initialText, useCaches);
    }

    public void addRef() {
        ++refCount;
    }

    public boolean willDispose() {
        return refCount == 1;
    }

    public void dispose() {
        --refCount;
        if (refCount == 0) {
            ErlideScanner.destroy(scannerName);
        }
    }

    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlideScanner.replaceText(scannerName, offset, removeLength, newText);
    }

    public ErlToken getTokenAt(final int offset) {
        return ErlideScanner.getTokenAt(scannerName, offset);
    }

}
