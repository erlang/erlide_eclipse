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

import org.erlide.core.text.ErlangToolkit;
import org.erlide.jinterface.backend.IDisposable;

import erlang.ErlideScanner;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IDisposable {
    private final String moduleName;
    private int refCount = 0;

    public ErlScanner(final IErlModule module, final String initialText,
            final String moduleFileName) {
        moduleName = ErlangToolkit.createScannerModuleName(module);
        ErlideScanner.initialScan(moduleName, moduleFileName, initialText,
                false);
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
            ErlideScanner.destroy(moduleName);
        }
    }

    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlideScanner.replaceText(moduleName, offset, removeLength, newText);
    }

    public ErlToken getTokenAt(final int offset) {
        return ErlideScanner.getTokenAt(moduleName, offset);
    }

    public String getScannerModuleName() {
        return moduleName;
    }

}
