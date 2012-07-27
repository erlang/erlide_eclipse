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
package org.erlide.core.internal.model.erlang;

import org.erlide.core.model.erlang.ErlToken;
import org.erlide.core.model.erlang.IErlScanner;
import org.erlide.utils.IDisposable;

/**
 * Erlang syntax scanner
 */
public class ErlScanner implements IDisposable, IErlScanner {
    private int refCount = 0;
    private final String name;

    public ErlScanner(final String name, final String initialText,
            final String path, final boolean useCaches) {
        this.name = name;
        // ErlLogger.debug("> create scanner " + name);
        ErlideScanner.initialScan(name, path, initialText, useCaches);
    }

    @Override
    public void addRef() {
        ++refCount;
        // ErlLogger.debug("> addref scanner " + name + " " + refCount);
    }

    @Override
    public boolean willDispose() {
        return refCount == 1;
    }

    @Override
    public void dispose() {
        --refCount;
        // ErlLogger.debug("> dispose scanner " + name + " " + refCount);
        if (refCount == 0) {
            // ErlLogger.debug("> destroy scanner " + name);
            ErlideScanner.destroy(name);
        }
    }

    @Override
    public void replaceText(final int offset, final int removeLength,
            final String newText) {
        ErlideScanner.replaceText(name, offset, removeLength, newText);
    }

    @Override
    public ErlToken getTokenAt(final int offset) {
        return ErlideScanner.getTokenAt(name, offset);
    }

    @Override
    public String getText() {
        return ErlideScanner.getText(name);
    }

    @Override
    public String getName() {
        return name;
    }
}
