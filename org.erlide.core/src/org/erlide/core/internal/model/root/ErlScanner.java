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
package org.erlide.core.internal.model.root;

import org.erlide.core.common.IDisposable;
import org.erlide.core.internal.model.erlang.ErlideScanner;
import org.erlide.core.model.root.ErlToken;
import org.erlide.core.model.root.IErlScanner;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IDisposable, IErlScanner {
    private int refCount = 0;
    private final String scannerName;

    public ErlScanner(final String scannerName, final String initialText,
            final String path, final boolean useCaches) {
        this.scannerName = scannerName;
        ErlideScanner.initialScan(scannerName, path, initialText, useCaches);
    }

    @Override
    public void addRef() {
        ++refCount;
    }

    @Override
    public boolean willDispose() {
        return refCount == 1;
    }

    @Override
    public void dispose() {
        --refCount;
        if (refCount == 0) {
            ErlideScanner.destroy(scannerName);
        }
    }

    @Override
    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlideScanner.replaceText(scannerName, offset, removeLength, newText);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return ErlideScanner.getTokenAt(scannerName, offset);
    }

    @Override
    public String getText() {
        return ErlideScanner.getText(scannerName);
    }

    @Override
    public String getName() {
        return scannerName;
    }
}
