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
package org.erlide.core.services.text;

import org.erlide.core.common.IDisposable;
import org.erlide.core.parsing.ErlToken;
import org.erlide.core.parsing.IErlScanner;

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

    /* (non-Javadoc)
     * @see org.erlide.core.parsing.IErlScanner#addRef()
     */
    public void addRef() {
        ++refCount;
    }

    /* (non-Javadoc)
     * @see org.erlide.core.parsing.IErlScanner#willDispose()
     */
    public boolean willDispose() {
        return refCount == 1;
    }

    /* (non-Javadoc)
     * @see org.erlide.core.parsing.IErlScanner#dispose()
     */
    public void dispose() {
        --refCount;
        if (refCount == 0) {
            ErlideScanner.destroy(scannerName);
        }
    }

    /* (non-Javadoc)
     * @see org.erlide.core.parsing.IErlScanner#replaceText(int, int, java.lang.String)
     */
    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlideScanner.replaceText(scannerName, offset, removeLength, newText);
    }

    /* (non-Javadoc)
     * @see org.erlide.core.parsing.IErlScanner#getTokenAt(int)
     */
    public ErlToken getTokenAt(final int offset) {
        return ErlideScanner.getTokenAt(scannerName, offset);
    }

    /* (non-Javadoc)
     * @see org.erlide.core.parsing.IErlScanner#getText()
     */
    public String getText() {
        return ErlideScanner.getText(scannerName);
    }

}
